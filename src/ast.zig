const std = @import("std");
const lex = @import("lex.zig");

pub const NodeArray = std.ArrayList(NodeKind);

pub const NodeKind = union(enum) {
    fn_decl: NodeFn,
    let_decl: NodeLet,
    if_stmt: NodeIf,
    return_stmt: NodeReturn,
    arg: NodeArg,
    literal_expr: NodeLiteral,
    binary_expr: NodeBinaryExpr,
};

pub const Type = enum {
    type_u32,
    type_i32,
};

pub const Operator = enum {
    plus,
    minus,
    star,
    slash,
};

pub const NodeLiteral = union(enum) {
    identifier: []const u8,
    integer: u32,
};

pub const NodeFn = struct {
    name: []const u8,
    args: NodeArray,
    fn_type: Type,
    body: NodeArray,
};

pub const NodeLet = struct {
    name: []const u8,
    let_type: Type,
    value: *NodeKind,
};

pub const NodeIf = struct {
    if_condition: *NodeKind,
    if_body: NodeArray,
    elif_nodes: ?NodeArray,
    else_body: ?NodeArray,
};

pub const NodeReturn = struct {
    value: *NodeKind,
};

pub const NodeArg = struct {
    name: []const u8,
    arg_type: Type,
};

pub const NodeBinaryExpr = struct {
    left: *NodeKind,
    operator: Operator,
    right: *NodeKind,
};
