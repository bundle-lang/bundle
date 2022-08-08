const std = @import("std");
const log = std.log;

const ast = @import("ast.zig");
const visitor = @import("visitor.zig");
const name_resolve = @import("name_resolve.zig");

const TypeChecker = struct {
    nodes: ast.NodeArray,
    decl_table: name_resolve.DeclTable,
    current_return_type: ?ast.Type,
    found_return_stmt: bool,

    fn inferType(self: *TypeChecker, node: *const ast.NodeKind) ast.Type {
        return switch (node.*) {
            .reference => |ref| switch (self.decl_table.get(ref.id).?) {
                .fn_decl => |decl| decl.fn_type,
                .let_stmt => |stmt| stmt.let_type,
                .parameter => |param| param.parameter_type,
                else => unreachable,
            },
            .grouping_expr => |expr| self.inferType(expr.expr),
            .literal_expr => |expr| switch (expr) {
                .integer => ast.Type{ .type_i32 = {} },
                .boolean => ast.Type{ .type_bool = {} },
            },
            .unary_expr => |expr| self.inferType(expr.expr),
            .binary_expr => |expr| self.inferType(expr.left),
            .call_expr => |expr| switch (self.inferType(expr.left_expr)) {
                .signature => |sign| sign.return_type.*,
                else => ast.Type{ .type_error = {} },
            },
            else => unreachable,
        };
    }

    pub fn visitFnDecl(self: *TypeChecker, node: ast.NodeFnDecl) void {
        self.current_return_type = node.fn_type.signature.return_type.*;
    }

    pub fn visitLetStmt(self: *TypeChecker, node: ast.NodeLetStmt) void {
        const let_type = node.let_type;
        const value_type = self.inferType(node.value);

        if (!let_type.matches(value_type)) {
            log.err("declaration `{}` and value `{}` types mismatch", .{ let_type, value_type });
        }
    }

    pub fn visitAssignStmt(self: *TypeChecker, node: ast.NodeAssignStmt) void {
        const left_expr_type = self.inferType(node.left_expr);
        const value_type = self.inferType(node.value);

        if (!left_expr_type.matches(value_type)) {
            log.err("assignee `{}` and value `{}` types mismatch", .{ left_expr_type, value_type });
        }
    }

    pub fn visitIfStmt(self: *TypeChecker, node: ast.NodeIfStmt) void {
        const if_condition_type = self.inferType(node.if_condition);

        if (!if_condition_type.matches(.type_bool)) {
            log.err("condition evaluates to `{}`, but a boolean expression was expected", .{if_condition_type});
        }
    }

    pub fn visitElifStmt(self: *TypeChecker, node: ast.NodeElifStmt) void {
        const elif_condition_type = self.inferType(node.elif_condition);

        if (!elif_condition_type.matches(.type_bool)) {
            log.err("condition evaluates to `{}`, but a boolean expression was expected", .{elif_condition_type});
        }
    }

    pub fn visitReturnStmt(self: *TypeChecker, node: ast.NodeReturnStmt) void {
        const return_stmt_type = self.inferType(node.value);
        const return_type = self.current_return_type.?;

        if (!return_stmt_type.matches(return_type)) {
            log.err("function `{}` and return `{}` types mismatch", .{ return_type, return_stmt_type });
        }

        self.found_return_stmt = true;
    }

    pub fn visitBinaryExpr(self: *TypeChecker, node: ast.NodeBinaryExpr) void {
        const left_type = self.inferType(node.left);
        const right_type = self.inferType(node.right);

        if (!left_type.matches(right_type)) {
            log.err("left expression `{}` and right expression `{}` types mismatch", .{ left_type, right_type });
        }
    }

    pub fn visitCallExpr(self: *TypeChecker, node: ast.NodeCallExpr) void {
        const left_expr_type = self.inferType(node.left_expr);

        if (left_expr_type != .signature) {
            log.err("{} is not a callable expression", .{left_expr_type});
            return;
        }

        const signature = left_expr_type.signature;

        if (node.arguments.items.len != signature.parameter_types.items.len) {
            log.err("{} parameters were expected, but {} were supplied", .{ signature.parameter_types.items.len, node.arguments.items.len });
            return;
        }

        for (node.arguments.items) |argument, i| {
            const argument_type = self.inferType(&argument);
            const parameter_type = signature.parameter_types.items[i];

            if (!argument_type.matches(parameter_type)) {
                log.err("argument `{}` and parameter `{}` types mismatch", .{ argument_type, parameter_type });
            }
        }
    }

    pub fn exitFnDecl(self: *TypeChecker, _: ast.NodeFnDecl) void {
        if (self.current_return_type != null and !self.found_return_stmt) {
            log.err("missing return statement", .{});
        }

        self.current_return_type = null;
        self.found_return_stmt = false;
    }

    pub fn check(self: *TypeChecker) void {
        const type_checker_visitor = visitor.new(self);
        visitor.traverseArray(self.nodes, type_checker_visitor);
    }
};

pub fn new(nodes: ast.NodeArray, decl_table: name_resolve.DeclTable) TypeChecker {
    return TypeChecker{
        .nodes = nodes,
        .decl_table = decl_table,
        .current_return_type = null,
        .found_return_stmt = false,
    };
}
