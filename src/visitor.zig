const ast = @import("ast.zig");

const Visitor = struct {
    impl: *anyopaque,

    visitFnDeclImpl: fn (*anyopaque, ast.NodeFnDecl) void,
    visitLetStmtImpl: fn (*anyopaque, ast.NodeLetStmt) void,
    visitAssignStmtImpl: fn (*anyopaque, ast.NodeAssignStmt) void,
    visitIfStmtImpl: fn (*anyopaque, ast.NodeIfStmt) void,
    visitElifStmtImpl: fn (*anyopaque, ast.NodeElifStmt) void,
    visitReturnStmtImpl: fn (*anyopaque, ast.NodeReturnStmt) void,
    visitArgImpl: fn (*anyopaque, ast.NodeArg) void,
    visitPrimaryExprImpl: fn (*anyopaque, ast.NodePrimaryExpr) void,
    visitUnaryExprImpl: fn (*anyopaque, ast.NodeUnaryExpr) void,
    visitBinaryExprImpl: fn (*anyopaque, ast.NodeBinaryExpr) void,
    visitCallExprImpl: fn (*anyopaque, ast.NodeCallExpr) void,

    fn newVisitImpl(comptime impl_type: type, comptime node_type: type, comptime to_call: anytype) fn (*anyopaque, node_type) void {
        const info = @typeInfo(impl_type);
        const alignment = info.Pointer.alignment;

        return (struct {
            pub fn visitImpl(self: *anyopaque, node: node_type) void {
                const typed_self = @ptrCast(impl_type, @alignCast(alignment, self));
                return @call(.{ .modifier = .always_inline }, to_call, .{ typed_self, node });
            }
        }).visitImpl;
    }

    pub fn init(impl: anytype) Visitor {
        const impl_type = @TypeOf(impl);
        const info = @typeInfo(impl_type);

        if (info != .Pointer) @compileError("expected pointer");
        if (info.Pointer.size != .One) @compileError("expected one sized pointer");

        return .{
            .impl = impl,

            .visitFnDeclImpl = Visitor.newVisitImpl(impl_type, ast.NodeFnDecl, info.Pointer.child.visitFnDecl),
            .visitLetStmtImpl = Visitor.newVisitImpl(impl_type, ast.NodeLetStmt, info.Pointer.child.visitLetStmt),
            .visitAssignStmtImpl = Visitor.newVisitImpl(impl_type, ast.NodeAssignStmt, info.Pointer.child.visitAssignStmt),
            .visitIfStmtImpl = Visitor.newVisitImpl(impl_type, ast.NodeIfStmt, info.Pointer.child.visitIfStmt),
            .visitElifStmtImpl = Visitor.newVisitImpl(impl_type, ast.NodeElifStmt, info.Pointer.child.visitElifStmt),
            .visitReturnStmtImpl = Visitor.newVisitImpl(impl_type, ast.NodeReturnStmt, info.Pointer.child.visitReturnStmt),
            .visitArgImpl = Visitor.newVisitImpl(impl_type, ast.NodeArg, info.Pointer.child.visitArg),
            .visitPrimaryExprImpl = Visitor.newVisitImpl(impl_type, ast.NodePrimaryExpr, info.Pointer.child.visitPrimaryExpr),
            .visitUnaryExprImpl = Visitor.newVisitImpl(impl_type, ast.NodeUnaryExpr, info.Pointer.child.visitUnaryExpr),
            .visitBinaryExprImpl = Visitor.newVisitImpl(impl_type, ast.NodeBinaryExpr, info.Pointer.child.visitBinaryExpr),
            .visitCallExprImpl = Visitor.newVisitImpl(impl_type, ast.NodeCallExpr, info.Pointer.child.visitCallExpr),
        };
    }

    inline fn visitFnDecl(self: Visitor, node: ast.NodeFnDecl) void {
        self.visitFnDeclImpl(self.impl, node);
    }

    inline fn visitLetStmt(self: Visitor, node: ast.NodeLetStmt) void {
        self.visitLetStmtImpl(self.impl, node);
    }

    inline fn visitAssignStmt(self: Visitor, node: ast.NodeAssignStmt) void {
        self.visitAssignStmtImpl(self.impl, node);
    }

    inline fn visitIfStmt(self: Visitor, node: ast.NodeIfStmt) void {
        self.visitIfStmtImpl(self.impl, node);
    }

    inline fn visitElifStmt(self: Visitor, node: ast.NodeElifStmt) void {
        self.visitElifStmtImpl(self.impl, node);
    }

    inline fn visitReturnStmt(self: Visitor, node: ast.NodeReturnStmt) void {
        self.visitReturnStmtImpl(self.impl, node);
    }

    inline fn visitArg(self: Visitor, node: ast.NodeArg) void {
        self.visitArgImpl(self.impl, node);
    }

    inline fn visitPrimaryExpr(self: Visitor, node: ast.NodePrimaryExpr) void {
        self.visitPrimaryExprImpl(self.impl, node);
    }

    inline fn visitUnaryExpr(self: Visitor, node: ast.NodeUnaryExpr) void {
        self.visitUnaryExprImpl(self.impl, node);
    }

    inline fn visitBinaryExpr(self: Visitor, node: ast.NodeBinaryExpr) void {
        self.visitBinaryExprImpl(self.impl, node);
    }

    inline fn visitCallExpr(self: Visitor, node: ast.NodeCallExpr) void {
        self.visitCallExprImpl(self.impl, node);
    }

    fn dispatch(self: Visitor, node: ast.NodeKind) void {
        return switch (node) {
            .fn_decl => |decl| self.visitFnDecl(decl),
            .let_stmt => |stmt| self.visitLetStmt(stmt),
            .assign_stmt => |stmt| self.visitAssignStmt(stmt),
            .if_stmt => |stmt| self.visitIfStmt(stmt),
            .elif_stmt => |stmt| self.visitElifStmt(stmt),
            .return_stmt => |stmt| self.visitReturnStmt(stmt),
            .arg => |arg| self.visitArg(arg),
            .primary_expr => |expr| self.visitPrimaryExpr(expr),
            .unary_expr => |expr| self.visitUnaryExpr(expr),
            .binary_expr => |expr| self.visitBinaryExpr(expr),
            .call_expr => |expr| self.visitCallExpr(expr),
        };
    }
};

pub fn traverseArray(array: ast.NodeArray, visitor: Visitor) void {
    for (array.items) |node| {
        traverse(node, visitor);
    }
}

pub fn traverse(node: ast.NodeKind, visitor: Visitor) void {
    visitor.dispatch(node);

    switch (node) {
        .fn_decl => |decl| {
            traverseArray(decl.args, visitor);
            traverseArray(decl.body, visitor);
        },
        .let_stmt => |stmt| {
            traverse(stmt.value.*, visitor);
        },
        .assign_stmt => |stmt| {
            traverse(stmt.left_expr.*, visitor);
            traverse(stmt.value.*, visitor);
        },
        .if_stmt => |stmt| {
            traverse(stmt.if_condition.*, visitor);
            traverseArray(stmt.if_body, visitor);
            if (stmt.elif_nodes) |nodes| traverseArray(nodes, visitor);
            if (stmt.else_body) |nodes| traverseArray(nodes, visitor);
        },
        .elif_stmt => |stmt| {
            traverse(stmt.elif_condition.*, visitor);
            traverseArray(stmt.elif_body, visitor);
        },
        .return_stmt => |stmt| {
            traverse(stmt.value.*, visitor);
        },
        .primary_expr => |expr| if (expr == .grouping) {
            traverse(expr.grouping.*, visitor);
        },
        .unary_expr => |expr| {
            traverse(expr.expr.*, visitor);
        },
        .binary_expr => |expr| {
            traverse(expr.left.*, visitor);
            traverse(expr.right.*, visitor);
        },
        .call_expr => |expr| {
            traverse(expr.left_expr.*, visitor);
            traverseArray(expr.args, visitor);
        },
        .arg => {},
    }
}
