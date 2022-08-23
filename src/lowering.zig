const llvm = @cImport({
    @cInclude("llvm-c/Core.h");
});

const std = @import("std");

const ast = @import("ast.zig");
const name_resolve = @import("name_resolve.zig");
const visitor = @import("visitor.zig");

const Allocator = std.mem.Allocator;
const ValueRefTable = std.AutoHashMap(ast.NodeId, llvm.LLVMValueRef);
const TypeRefArray = std.ArrayList(llvm.LLVMTypeRef);

const LoweringContext = struct {
    allocator: Allocator,
    nodes: ast.NodeArray,

    decl_table: name_resolve.DeclTable,
    value_ref_table: ValueRefTable,

    context: llvm.LLVMContextRef,
    module: llvm.LLVMModuleRef,
    builder: llvm.LLVMBuilderRef,

    fn lowerString(self: *LoweringContext, str: []const u8) [:0]const u8 {
        return self.allocator.dupeZ(u8, str) catch unreachable;
    }

    fn lowerType(self: *LoweringContext, node_type: ast.Type) llvm.LLVMTypeRef {
        return switch (node_type) {
            .type_i32 => llvm.LLVMInt32TypeInContext(self.context),
            else => unreachable,
        };
    }

    fn lowerExpr(self: *LoweringContext, node: *const ast.NodeKind, load: bool) llvm.LLVMValueRef {
        return switch (node.*) {
            .reference => |ref| {
                const llvm_reference = self.lowerReference(ref);
                if (load) {
                    return llvm.LLVMBuildLoad(self.builder, llvm_reference, "tmp.load");
                } else return llvm_reference;
            },
            .grouping_expr => |expr| self.lowerExpr(expr.expr, load),
            .literal_expr => |expr| self.lowerLiteralExpr(expr),
            .unary_expr => |expr| self.lowerUnaryExpr(expr),
            .binary_expr => |expr| self.lowerBinaryExpr(expr),
            else => unreachable,
        };
    }

    fn lowerReference(self: *LoweringContext, node: ast.NodeReference) llvm.LLVMValueRef {
        return switch (self.decl_table.get(node.id).?) {
            .let_stmt => |stmt| self.value_ref_table.get(stmt.id).?,
            else => unreachable,
        };
    }

    fn lowerLiteralExpr(self: *LoweringContext, node: ast.NodeLiteralExpr) llvm.LLVMValueRef {
        return switch (node) {
            .integer => |integer| llvm.LLVMConstInt(self.lowerType(.type_i32), integer, @boolToInt(false)),
            else => unreachable,
        };
    }

    fn lowerUnaryExpr(self: *LoweringContext, node: ast.NodeUnaryExpr) llvm.LLVMValueRef {
        const llvm_expr = self.lowerExpr(node.expr, true);

        return switch (node.operator) {
            .plus => llvm_expr,
            .minus => llvm.LLVMBuildNeg(self.builder, llvm_expr, "tmp.neg"),
            else => unreachable,
        };
    }

    fn lowerBinaryExpr(self: *LoweringContext, node: ast.NodeBinaryExpr) llvm.LLVMValueRef {
        const llvm_left_expr = self.lowerExpr(node.left, true);
        const llvm_right_expr = self.lowerExpr(node.right, true);

        return switch (node.operator) {
            .plus => llvm.LLVMBuildAdd(self.builder, llvm_left_expr, llvm_right_expr, "tmp.add"),
            .minus => llvm.LLVMBuildSub(self.builder, llvm_left_expr, llvm_right_expr, "tmp.sub"),
            .star => llvm.LLVMBuildMul(self.builder, llvm_left_expr, llvm_right_expr, "tmp.mul"),
            else => unreachable,
        };
    }

    pub fn visitFnDecl(self: *LoweringContext, node: ast.NodeFnDecl) void {
        var llvm_param_types = TypeRefArray.init(self.allocator);
        for (node.parameters.items) |param| {
            llvm_param_types.append(self.lowerType(param.parameter.parameter_type)) catch unreachable;
        }

        const llvm_function = llvm.LLVMAddFunction(self.module, self.lowerString(node.name), llvm.LLVMFunctionType(
            self.lowerType(node.fn_type.signature.return_type.*),
            llvm_param_types.items.ptr,
            @intCast(c_uint, llvm_param_types.items.len),
            @boolToInt(false),
        ));

        const llvm_function_block = llvm.LLVMAppendBasicBlockInContext(self.context, llvm_function, "fn.entry");
        llvm.LLVMPositionBuilderAtEnd(self.builder, llvm_function_block);
    }

    pub fn visitLetStmt(self: *LoweringContext, node: ast.NodeLetStmt) void {
        const llvm_let_alloca = llvm.LLVMBuildAlloca(self.builder, self.lowerType(node.let_type), self.lowerString(node.name));
        const llvm_let_value = self.lowerExpr(node.value, true);

        _ = llvm.LLVMBuildStore(self.builder, llvm_let_value, llvm_let_alloca);

        self.value_ref_table.put(node.id, llvm_let_alloca) catch unreachable;
    }

    pub fn visitAssignStmt(self: *LoweringContext, node: ast.NodeAssignStmt) void {
        _ = llvm.LLVMBuildStore(self.builder, self.lowerExpr(node.value, true), self.lowerExpr(node.left_expr, false));
    }

    pub fn visitReturnStmt(self: *LoweringContext, node: ast.NodeReturnStmt) void {
        _ = llvm.LLVMBuildRet(self.builder, self.lowerExpr(node.value, true));
    }

    pub fn lower(self: *LoweringContext) void {
        defer llvm.LLVMContextDispose(self.context);
        defer llvm.LLVMDisposeModule(self.module);
        defer llvm.LLVMDisposeBuilder(self.builder);

        const lowering_context_visitor = visitor.new(self);
        visitor.traverseArray(self.nodes, lowering_context_visitor);

        llvm.LLVMDumpModule(self.module);
    }
};

pub fn new(allocator: Allocator, nodes: ast.NodeArray, decl_table: name_resolve.DeclTable) LoweringContext {
    const context = llvm.LLVMContextCreate();

    return LoweringContext{
        .allocator = allocator,
        .nodes = nodes,

        .decl_table = decl_table,
        .value_ref_table = ValueRefTable.init(allocator),

        .context = context,
        .module = llvm.LLVMModuleCreateWithNameInContext("main", context),
        .builder = llvm.LLVMCreateBuilderInContext(context),
    };
}
