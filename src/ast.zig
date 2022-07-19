const std = @import("std");
const lex = @import("lex.zig");

pub const NodeArray = std.ArrayList(NodeKind);

pub const NodeKind = union(enum) {
    fn_decl: NodeFn,
    let_decl: NodeLet,
    if_stmt: NodeIf,
    elif_stmt: NodeElif,
    return_stmt: NodeReturn,
    arg: NodeArg,
    basic_expr: NodeBasicExpr,
    binary_expr: NodeBinaryExpr,
};

pub const Type = enum {
    type_u32,
    type_i32,
    type_bool,
};

pub const Operator = enum {
    plus,
    minus,
    star,
    slash,
};

pub const NodeBasicExpr = union(enum) {
    identifier: []const u8,
    integer: u32,
    boolean: bool,
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

pub const NodeElif = struct {
    elif_condition: *NodeKind,
    elif_body: NodeArray,
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
