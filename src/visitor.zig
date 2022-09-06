const ast = @import("ast.zig");

const Visitor = struct {
    impl: *anyopaque,

    visitFnDeclImpl: ?fn (*anyopaque, ast.NodeFnDecl) void,
    visitExternDeclImpl: ?fn (*anyopaque, ast.NodeExternDecl) void,
    visitLetStmtImpl: ?fn (*anyopaque, ast.NodeLetStmt) void,
    visitAssignStmtImpl: ?fn (*anyopaque, ast.NodeAssignStmt) void,
    visitIfStmtImpl: ?fn (*anyopaque, ast.NodeIfStmt) void,
    visitElifStmtImpl: ?fn (*anyopaque, ast.NodeElifStmt) void,
    visitReturnStmtImpl: ?fn (*anyopaque, ast.NodeReturnStmt) void,
    visitParameterImpl: ?fn (*anyopaque, ast.NodeParameter) void,
    visitReferenceImpl: ?fn (*anyopaque, ast.NodeReference) void,
    visitGroupingExprImpl: ?fn (*anyopaque, ast.NodeGroupingExpr) void,
    visitLiteralExprImpl: ?fn (*anyopaque, ast.NodeLiteralExpr) void,
    visitUnaryExprImpl: ?fn (*anyopaque, ast.NodeUnaryExpr) void,
    visitBinaryExprImpl: ?fn (*anyopaque, ast.NodeBinaryExpr) void,
    visitCallExprImpl: ?fn (*anyopaque, ast.NodeCallExpr) void,

    enterBlockStmtImpl: ?fn (*anyopaque, ast.NodeBlockStmt) void,
    exitBlockStmtImpl: ?fn (*anyopaque, ast.NodeBlockStmt) void,
    exitFnDeclImpl: ?fn (*anyopaque, ast.NodeFnDecl) void,

    inline fn visitFnDecl(self: Visitor, node: ast.NodeFnDecl) void {
        if (self.visitFnDeclImpl) |visitImpl| visitImpl(self.impl, node);
    }

    inline fn visitExternDecl(self: Visitor, node: ast.NodeExternDecl) void {
        if (self.visitExternDeclImpl) |visitImpl| visitImpl(self.impl, node);
    }

    inline fn visitLetStmt(self: Visitor, node: ast.NodeLetStmt) void {
        if (self.visitLetStmtImpl) |visitImpl| visitImpl(self.impl, node);
    }

    inline fn visitAssignStmt(self: Visitor, node: ast.NodeAssignStmt) void {
        if (self.visitAssignStmtImpl) |visitImpl| visitImpl(self.impl, node);
    }

    inline fn visitIfStmt(self: Visitor, node: ast.NodeIfStmt) void {
        if (self.visitIfStmtImpl) |visitImpl| visitImpl(self.impl, node);
    }

    inline fn visitElifStmt(self: Visitor, node: ast.NodeElifStmt) void {
        if (self.visitElifStmtImpl) |visitImpl| visitImpl(self.impl, node);
    }

    inline fn visitReturnStmt(self: Visitor, node: ast.NodeReturnStmt) void {
        if (self.visitReturnStmtImpl) |visitImpl| visitImpl(self.impl, node);
    }

    inline fn visitParameter(self: Visitor, node: ast.NodeParameter) void {
        if (self.visitParameterImpl) |visitImpl| visitImpl(self.impl, node);
    }

    inline fn visitReference(self: Visitor, node: ast.NodeReference) void {
        if (self.visitReferenceImpl) |visitImpl| visitImpl(self.impl, node);
    }

    inline fn visitGroupingExpr(self: Visitor, node: ast.NodeGroupingExpr) void {
        if (self.visitGroupingExprImpl) |visitImpl| visitImpl(self.impl, node);
    }

    inline fn visitLiteralExpr(self: Visitor, node: ast.NodeLiteralExpr) void {
        if (self.visitLiteralExprImpl) |visitImpl| visitImpl(self.impl, node);
    }

    inline fn visitUnaryExpr(self: Visitor, node: ast.NodeUnaryExpr) void {
        if (self.visitUnaryExprImpl) |visitImpl| visitImpl(self.impl, node);
    }

    inline fn visitBinaryExpr(self: Visitor, node: ast.NodeBinaryExpr) void {
        if (self.visitBinaryExprImpl) |visitImpl| visitImpl(self.impl, node);
    }

    inline fn visitCallExpr(self: Visitor, node: ast.NodeCallExpr) void {
        if (self.visitCallExprImpl) |visitImpl| visitImpl(self.impl, node);
    }

    inline fn enterBlockStmt(self: Visitor, node: ast.NodeBlockStmt) void {
        if (self.enterBlockStmtImpl) |enterImpl| enterImpl(self.impl, node);
    }

    inline fn exitBlockStmt(self: Visitor, node: ast.NodeBlockStmt) void {
        if (self.exitBlockStmtImpl) |exitImpl| exitImpl(self.impl, node);
    }

    inline fn exitFnDecl(self: Visitor, node: ast.NodeFnDecl) void {
        if (self.exitFnDeclImpl) |exitImpl| exitImpl(self.impl, node);
    }

    fn dispatch(self: Visitor, node: ast.NodeKind) void {
        return switch (node) {
            .fn_decl => |decl| self.visitFnDecl(decl),
            .extern_decl => |decl| self.visitExternDecl(decl),
            .let_stmt => |stmt| self.visitLetStmt(stmt),
            .assign_stmt => |stmt| self.visitAssignStmt(stmt),
            .if_stmt => |stmt| self.visitIfStmt(stmt),
            .elif_stmt => |stmt| self.visitElifStmt(stmt),
            .return_stmt => |stmt| self.visitReturnStmt(stmt),
            .parameter => |param| self.visitParameter(param),
            .reference => |ref| self.visitReference(ref),
            .grouping_expr => |expr| self.visitGroupingExpr(expr),
            .literal_expr => |expr| self.visitLiteralExpr(expr),
            .unary_expr => |expr| self.visitUnaryExpr(expr),
            .binary_expr => |expr| self.visitBinaryExpr(expr),
            .call_expr => |expr| self.visitCallExpr(expr),
            .block_stmt => {},
        };
    }
};

fn newVisitImpl(comptime impl_type: type, comptime node_type: type, comptime to_call: []const u8) ?fn (*anyopaque, node_type) void {
    const info = @typeInfo(impl_type);

    if (!@hasDecl(Visitor, to_call)) {
        @compileError(to_call ++ " is not a visitable method");
    }
    if (!@hasDecl(info.Pointer.child, to_call)) {
        return null;
    }

    const alignment = info.Pointer.alignment;
    const to_call_fn = @field(info.Pointer.child, to_call);

    return (struct {
        fn visitImpl(self: *anyopaque, node: node_type) void {
            const typed_self = @ptrCast(impl_type, @alignCast(alignment, self));
            return @call(.{ .modifier = .always_inline }, to_call_fn, .{ typed_self, node });
        }
    }).visitImpl;
}

pub fn new(impl: anytype) Visitor {
    const impl_type = @TypeOf(impl);
    const info = @typeInfo(impl_type);

    if (info != .Pointer) @compileError("expected pointer");
    if (info.Pointer.size != .One) @compileError("expected one sized pointer");

    return .{
        .impl = impl,

        .visitFnDeclImpl = newVisitImpl(impl_type, ast.NodeFnDecl, "visitFnDecl"),
        .visitExternDeclImpl = newVisitImpl(impl_type, ast.NodeExternDecl, "visitExternDecl"),
        .visitLetStmtImpl = newVisitImpl(impl_type, ast.NodeLetStmt, "visitLetStmt"),
        .visitAssignStmtImpl = newVisitImpl(impl_type, ast.NodeAssignStmt, "visitAssignStmt"),
        .visitIfStmtImpl = newVisitImpl(impl_type, ast.NodeIfStmt, "visitIfStmt"),
        .visitElifStmtImpl = newVisitImpl(impl_type, ast.NodeElifStmt, "visitElifStmt"),
        .visitReturnStmtImpl = newVisitImpl(impl_type, ast.NodeReturnStmt, "visitReturnStmt"),
        .visitParameterImpl = newVisitImpl(impl_type, ast.NodeParameter, "visitParameter"),
        .visitReferenceImpl = newVisitImpl(impl_type, ast.NodeReference, "visitReference"),
        .visitGroupingExprImpl = newVisitImpl(impl_type, ast.NodeGroupingExpr, "visitGroupingExpr"),
        .visitLiteralExprImpl = newVisitImpl(impl_type, ast.NodeLiteralExpr, "visitLiteralExpr"),
        .visitUnaryExprImpl = newVisitImpl(impl_type, ast.NodeUnaryExpr, "visitUnaryExpr"),
        .visitBinaryExprImpl = newVisitImpl(impl_type, ast.NodeBinaryExpr, "visitBinaryExpr"),
        .visitCallExprImpl = newVisitImpl(impl_type, ast.NodeCallExpr, "visitCallExpr"),

        .enterBlockStmtImpl = newVisitImpl(impl_type, ast.NodeBlockStmt, "enterBlockStmt"),
        .exitBlockStmtImpl = newVisitImpl(impl_type, ast.NodeBlockStmt, "exitBlockStmt"),
        .exitFnDeclImpl = newVisitImpl(impl_type, ast.NodeFnDecl, "exitFnDecl"),
    };
}

pub fn traverseArray(array: ast.NodeArray, visitor: Visitor) void {
    for (array.items) |node| {
        traverse(&node, visitor);
    }
}

pub fn traverseBlock(block: ast.NodeBlockStmt, visitor: Visitor) void {
    traverseArray(block.list, visitor);
    visitor.exitBlockStmt(block);
}

pub fn traverse(node: *const ast.NodeKind, visitor: Visitor) void {
    visitor.dispatch(node.*);

    switch (node.*) {
        .block_stmt => |block| {
            visitor.enterBlockStmt(block);
            traverseBlock(block, visitor);
        },
        .fn_decl => |decl| {
            visitor.enterBlockStmt(decl.body);
            traverseArray(decl.parameters, visitor);
            traverseBlock(decl.body, visitor);

            visitor.exitFnDecl(decl);
        },
        .let_stmt => |stmt| {
            traverse(stmt.value, visitor);
        },
        .assign_stmt => |stmt| {
            traverse(stmt.left_expr, visitor);
            traverse(stmt.value, visitor);
        },
        .if_stmt => |stmt| {
            visitor.enterBlockStmt(stmt.if_body);
            traverse(stmt.if_condition, visitor);
            traverseBlock(stmt.if_body, visitor);

            if (stmt.elif_nodes) |nodes| traverseArray(nodes, visitor);

            if (stmt.else_body) |body| {
                visitor.enterBlockStmt(body);
                traverseBlock(body, visitor);
            }
        },
        .elif_stmt => |stmt| {
            visitor.enterBlockStmt(stmt.elif_body);
            traverse(stmt.elif_condition, visitor);
            traverseBlock(stmt.elif_body, visitor);
        },
        .return_stmt => |stmt| {
            traverse(stmt.value, visitor);
        },
        .grouping_expr => |expr| {
            traverse(expr.expr, visitor);
        },
        .unary_expr => |expr| {
            traverse(expr.expr, visitor);
        },
        .binary_expr => |expr| {
            traverse(expr.left, visitor);
            traverse(expr.right, visitor);
        },
        .call_expr => |expr| {
            traverse(expr.left_expr, visitor);
            traverseArray(expr.arguments, visitor);
        },
        .reference, .literal_expr, .parameter, .extern_decl => {},
    }
}
