const std = @import("std");
const log = std.log;

const ast = @import("ast.zig");
const visitor = @import("visitor.zig");
const name_resolve = @import("name_resolve.zig");

const TypeChecker = struct {
    nodes: ast.NodeArray,
    decl_table: name_resolve.DeclTable,
    current_function_type: ?ast.Type,
    found_return_stmt: bool,

    fn inferType(self: *TypeChecker, node: *ast.NodeKind) ast.Type {
        return switch (node.*) {
            .reference => |ref| switch (self.decl_table.table.get(ref.id).?) {
                .fn_decl => |decl| decl.fn_type,
                .let_stmt => |stmt| stmt.let_type,
                .arg => |arg| arg.arg_type,
                else => unreachable,
            },
            .grouping_expr => |expr| self.inferType(expr.expr),
            .literal_expr => |expr| switch (expr) {
                .integer => ast.Type.type_i32,
                .boolean => ast.Type.type_bool,
            },
            .unary_expr => |expr| self.inferType(expr.expr),
            .binary_expr => |expr| self.inferType(expr.left),
            .call_expr => |expr| self.inferType(expr.left_expr),
            else => unreachable,
        };
    }

    pub fn visitFnDecl(self: *TypeChecker, node: ast.NodeFnDecl) void {
        self.current_function_type = node.fn_type;
    }

    pub fn visitLetStmt(self: *TypeChecker, node: ast.NodeLetStmt) void {
        const let_type = node.let_type;
        const value_type = self.inferType(node.value);

        if (let_type != value_type) {
            log.err("mismatched types {} and {}", .{ let_type, value_type });
        }
    }

    pub fn visitAssignStmt(self: *TypeChecker, node: ast.NodeAssignStmt) void {
        const left_expr_type = self.inferType(node.left_expr);
        const value_type = self.inferType(node.value);

        if (left_expr_type != value_type) {
            log.err("mismatched types {} and {}", .{ left_expr_type, value_type });
        }
    }

    pub fn visitIfStmt(self: *TypeChecker, node: ast.NodeIfStmt) void {
        const if_condition_type = self.inferType(node.if_condition);

        if (if_condition_type != .type_bool) {
            log.err("condition must evaluate to a boolean", .{});
        }
    }

    pub fn visitElifStmt(self: *TypeChecker, node: ast.NodeElifStmt) void {
        const elif_condition_type = self.inferType(node.elif_condition);

        if (elif_condition_type != .type_bool) {
            log.err("condition must evaluate to a boolean", .{});
        }
    }

    pub fn visitReturnStmt(self: *TypeChecker, node: ast.NodeReturnStmt) void {
        const return_stmt_type = self.inferType(node.value);

        if (return_stmt_type != self.current_function_type.?) {
            log.err("condition must evaluate to a boolean", .{});
        }

        self.found_return_stmt = true;
    }

    pub fn visitBinaryExpr(self: *TypeChecker, node: ast.NodeBinaryExpr) void {
        const left_type = self.inferType(node.left);
        const right_type = self.inferType(node.right);

        if (left_type != right_type) {
            log.err("mismatched types {} and {}", .{ left_type, right_type });
        }
    }

    pub fn exitFnDecl(self: *TypeChecker, _: ast.NodeFnDecl) void {
        if (self.current_function_type != null and !self.found_return_stmt) {
            log.err("missing return statement", .{});
        }

        self.current_function_type = null;
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
        .current_function_type = null,
        .found_return_stmt = false,
    };
}
