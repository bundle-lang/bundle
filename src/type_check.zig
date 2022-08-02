const std = @import("std");
const log = std.log;

const parse = @import("parse.zig");
const ast = @import("ast.zig");
const visitor = @import("visitor.zig");

const TypeChecker = struct {
    nodes: ast.NodeArray,

    fn inferType(self: *TypeChecker, node: *ast.NodeKind) ast.Type {
        return switch (node.*) {
            .primary_expr => |expr| switch (expr) {
                .integer => ast.Type.type_i32,
                .boolean => ast.Type.type_bool,
                .grouping => self.inferType(expr.grouping),
                else => unreachable,
            },
            .unary_expr => |expr| self.inferType(expr.expr),
            .binary_expr => |expr| self.inferType(expr.left),
            else => unreachable,
        };
    }

    pub fn visitLetStmt(self: *TypeChecker, node: ast.NodeLetStmt) void {
        const let_type = node.let_type;
        const value_type = self.inferType(node.value);

        if (let_type != value_type) {
            log.err("mismatched types {} and {}", .{ let_type, value_type });
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

    pub fn visitBinaryExpr(self: *TypeChecker, node: ast.NodeBinaryExpr) void {
        const left_type = self.inferType(node.left);
        const right_type = self.inferType(node.right);

        if (left_type != right_type) {
            log.err("mismatched types {} and {}", .{ left_type, right_type });
        }
    }

    pub fn check(self: *TypeChecker) void {
        const type_checker_visitor = visitor.new(self);
        visitor.traverseArray(self.nodes, type_checker_visitor);
    }
};

pub fn new(nodes: ast.NodeArray) TypeChecker {
    return TypeChecker{ .nodes = nodes };
}
