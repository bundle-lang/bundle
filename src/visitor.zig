const ast = @import("ast.zig");

const Visitor = struct {
    impl: *anyopaque,

    visitFnDeclImpl: fn (*anyopaque, ast.NodeKind) void,
    visitLetStmtImpl: fn (*anyopaque, ast.NodeKind) void,
    visitAssignStmtImpl: fn (*anyopaque, ast.NodeKind) void,
    visitIfStmtImpl: fn (*anyopaque, ast.NodeKind) void,
    visitElifStmtImpl: fn (*anyopaque, ast.NodeKind) void,
    visitReturnStmtImpl: fn (*anyopaque, ast.NodeKind) void,
    visitArgImpl: fn (*anyopaque, ast.NodeKind) void,
    visitPrimaryExprImpl: fn (*anyopaque, ast.NodeKind) void,
    visitUnaryExprImpl: fn (*anyopaque, ast.NodeKind) void,
    visitBinaryExprImpl: fn (*anyopaque, ast.NodeKind) void,
    visitCallExprImpl: fn (*anyopaque, ast.NodeKind) void,

    fn newVisitImpl(comptime impl_type: type, comptime to_call: anytype) fn (*anyopaque, ast.NodeKind) void {
        const info = @typeInfo(impl_type);
        const alignment = info.Pointer.alignment;

        return (struct {
            pub fn visitImpl(self: *anyopaque, node: ast.NodeKind) void {
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

            .visitFnDeclImpl = Visitor.newVisitImpl(impl_type, info.Pointer.child.visitFnDecl),
            .visitLetStmtImpl = Visitor.newVisitImpl(impl_type, info.Pointer.child.visitLetStmt),
            .visitAssignStmtImpl = Visitor.newVisitImpl(impl_type, info.Pointer.child.visitAssignStmt),
            .visitIfStmtImpl = Visitor.newVisitImpl(impl_type, info.Pointer.child.visitIfStmt),
            .visitElifStmtImpl = Visitor.newVisitImpl(impl_type, info.Pointer.child.visitElifStmt),
            .visitReturnStmtImpl = Visitor.newVisitImpl(impl_type, info.Pointer.child.visitReturnStmt),
            .visitArgImpl = Visitor.newVisitImpl(impl_type, info.Pointer.child.visitArg),
            .visitPrimaryExprImpl = Visitor.newVisitImpl(impl_type, info.Pointer.child.visitPrimaryExpr),
            .visitUnaryExprImpl = Visitor.newVisitImpl(impl_type, info.Pointer.child.visitUnaryExpr),
            .visitBinaryExprImpl = Visitor.newVisitImpl(impl_type, info.Pointer.child.visitBinaryExpr),
            .visitCallExprImpl = Visitor.newVisitImpl(impl_type, info.Pointer.child.visitCallExpr),
        };
    }

    inline fn visitFnDecl(self: Visitor, node: ast.NodeKind) void {
        self.visitFnDeclImpl(self.impl, node);
    }

    inline fn visitLetStmt(self: Visitor, node: ast.NodeKind) void {
        self.visitLetStmtImpl(self.impl, node);
    }

    inline fn visitAssignStmt(self: Visitor, node: ast.NodeKind) void {
        self.visitAssignStmtImpl(self.impl, node);
    }

    inline fn visitIfStmt(self: Visitor, node: ast.NodeKind) void {
        self.visitIfStmtImpl(self.impl, node);
    }

    inline fn visitElifStmt(self: Visitor, node: ast.NodeKind) void {
        self.visitElifStmtImpl(self.impl, node);
    }

    inline fn visitReturnStmt(self: Visitor, node: ast.NodeKind) void {
        self.visitReturnStmtImpl(self.impl, node);
    }

    inline fn visitArg(self: Visitor, node: ast.NodeKind) void {
        self.visitArgImpl(self.impl, node);
    }

    inline fn visitPrimaryExpr(self: Visitor, node: ast.NodeKind) void {
        self.visitPrimaryExprImpl(self.impl, node);
    }

    inline fn visitUnaryExpr(self: Visitor, node: ast.NodeKind) void {
        self.visitUnaryExprImpl(self.impl, node);
    }

    inline fn visitBinaryExpr(self: Visitor, node: ast.NodeKind) void {
        self.visitBinaryExprImpl(self.impl, node);
    }

    inline fn visitCallExpr(self: Visitor, node: ast.NodeKind) void {
        self.visitCallExprImpl(self.impl, node);
    }

    fn dispatch(self: Visitor, node: ast.NodeKind) void {
        return switch (node) {
            .fn_decl => self.visitFnDecl(node),
            .let_stmt => self.visitLetStmt(node),
            .assign_stmt => self.visitAssignStmt(node),
            .if_stmt => self.visitIfStmt(node),
            .elif_stmt => self.visitElifStmt(node),
            .return_stmt => self.visitReturnStmt(node),
            .arg => self.visitArg(node),
            .primary_expr => self.visitPrimaryExpr(node),
            .unary_expr => self.visitUnaryExpr(node),
            .binary_expr => self.visitBinaryExpr(node),
            .call_expr => self.visitCallExpr(node),
        };
    }
};
