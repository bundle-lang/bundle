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
const ValueRefArray = std.ArrayList(llvm.LLVMValueRef);

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
            .type_bool => llvm.LLVMInt1TypeInContext(self.context),
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
            .call_expr => |expr| self.lowerCallExpr(expr),
            else => unreachable,
        };
    }

    fn lowerFnDecl(self: *LoweringContext, node: ast.NodeFnDecl) void {
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

        for (node.parameters.items) |param, i| {
            const llvm_param_alloca = llvm.LLVMBuildAlloca(
                self.builder,
                self.lowerType(param.parameter.parameter_type),
                self.lowerString(param.parameter.name),
            );
            _ = llvm.LLVMBuildStore(self.builder, llvm.LLVMGetParam(llvm_function, @intCast(c_uint, i)), llvm_param_alloca);

            self.value_ref_table.put(param.parameter.id, llvm_param_alloca) catch unreachable;
        }

        self.dispatchArray(node.body.list);
    }

    fn lowerLetStmt(self: *LoweringContext, node: ast.NodeLetStmt) void {
        const llvm_let_alloca = llvm.LLVMBuildAlloca(self.builder, self.lowerType(node.let_type), self.lowerString(node.name));
        const llvm_let_value = self.lowerExpr(node.value, true);

        _ = llvm.LLVMBuildStore(self.builder, llvm_let_value, llvm_let_alloca);

        self.value_ref_table.put(node.id, llvm_let_alloca) catch unreachable;
    }

    fn lowerAssignStmt(self: *LoweringContext, node: ast.NodeAssignStmt) void {
        _ = llvm.LLVMBuildStore(self.builder, self.lowerExpr(node.value, true), self.lowerExpr(node.left_expr, false));
    }

    fn lowerIfStmt(self: *LoweringContext, node: ast.NodeIfStmt) void {
        const llvm_current_function = llvm.LLVMGetBasicBlockParent(llvm.LLVMGetInsertBlock(self.builder));

        const llvm_if_condition = self.lowerExpr(node.if_condition, true);
        const llvm_if_then_block = llvm.LLVMAppendBasicBlockInContext(self.context, llvm_current_function, "if.then");
        var llvm_else_block = llvm.LLVMCreateBasicBlockInContext(self.context, "if.else");
        const llvm_merge_block = llvm.LLVMCreateBasicBlockInContext(self.context, "if.merge");

        _ = llvm.LLVMBuildCondBr(self.builder, llvm_if_condition, llvm_if_then_block, llvm_else_block);

        llvm.LLVMPositionBuilderAtEnd(self.builder, llvm_if_then_block);
        self.dispatchArray(node.if_body.list);

        _ = llvm.LLVMBuildBr(self.builder, llvm_merge_block);

        if (node.elif_nodes) |elif_nodes| {
            for (elif_nodes.items) |elif_node| {
                llvm.LLVMAppendExistingBasicBlock(llvm_current_function, llvm_else_block);
                llvm.LLVMPositionBuilderAtEnd(self.builder, llvm_else_block);

                const llvm_elif_condition = self.lowerExpr(elif_node.elif_stmt.elif_condition, true);
                const llvm_elif_then_block = llvm.LLVMAppendBasicBlockInContext(self.context, llvm_current_function, "elif.then");
                llvm_else_block = llvm.LLVMCreateBasicBlockInContext(self.context, "elif.else");

                _ = llvm.LLVMBuildCondBr(self.builder, llvm_elif_condition, llvm_elif_then_block, llvm_else_block);

                llvm.LLVMPositionBuilderAtEnd(self.builder, llvm_elif_then_block);
                self.dispatchArray(elif_node.elif_stmt.elif_body.list);

                _ = llvm.LLVMBuildBr(self.builder, llvm_merge_block);
            }
        }

        llvm.LLVMAppendExistingBasicBlock(llvm_current_function, llvm_else_block);
        llvm.LLVMPositionBuilderAtEnd(self.builder, llvm_else_block);
        if (node.else_body) |else_body| {
            self.dispatchArray(else_body.list);
        }

        _ = llvm.LLVMBuildBr(self.builder, llvm_merge_block);

        llvm.LLVMAppendExistingBasicBlock(llvm_current_function, llvm_merge_block);
        llvm.LLVMPositionBuilderAtEnd(self.builder, llvm_merge_block);
    }

    fn lowerReturnStmt(self: *LoweringContext, node: ast.NodeReturnStmt) void {
        _ = llvm.LLVMBuildRet(self.builder, self.lowerExpr(node.value, true));
    }

    fn lowerReference(self: *LoweringContext, node: ast.NodeReference) llvm.LLVMValueRef {
        return switch (self.decl_table.get(node.id).?) {
            .let_stmt => |stmt| self.value_ref_table.get(stmt.id).?,
            .parameter => |param| self.value_ref_table.get(param.id).?,
            else => unreachable,
        };
    }

    fn lowerLiteralExpr(self: *LoweringContext, node: ast.NodeLiteralExpr) llvm.LLVMValueRef {
        return switch (node) {
            .integer => |integer| llvm.LLVMConstInt(self.lowerType(.type_i32), integer, @boolToInt(false)),
            .boolean => |boolean| llvm.LLVMConstInt(self.lowerType(.type_bool), @boolToInt(boolean), @boolToInt(false)),
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

    fn lowerCallExpr(self: *LoweringContext, node: ast.NodeCallExpr) llvm.LLVMValueRef {
        const callee_name = node.left_expr.reference.name;
        const llvm_callee = llvm.LLVMGetNamedFunction(self.module, self.lowerString(callee_name));

        var llvm_arg_values = ValueRefArray.init(self.allocator);
        for (node.arguments.items) |arg| {
            llvm_arg_values.append(self.lowerExpr(&arg, true)) catch unreachable;
        }

        return llvm.LLVMBuildCall(
            self.builder,
            llvm_callee,
            llvm_arg_values.items.ptr,
            @intCast(c_uint, llvm_arg_values.items.len),
            "tmp.call",
        );
    }

    fn dispatchArray(self: *LoweringContext, nodes: ast.NodeArray) void {
        for (nodes.items) |node| {
            self.dispatch(node);
        }
    }

    fn dispatch(self: *LoweringContext, node: ast.NodeKind) void {
        switch (node) {
            .fn_decl => |decl| self.lowerFnDecl(decl),
            .let_stmt => |stmt| self.lowerLetStmt(stmt),
            .assign_stmt => |stmt| self.lowerAssignStmt(stmt),
            .if_stmt => |stmt| self.lowerIfStmt(stmt),
            .return_stmt => |stmt| self.lowerReturnStmt(stmt),
            else => unreachable,
        }
    }

    pub fn lower(self: *LoweringContext) void {
        defer llvm.LLVMContextDispose(self.context);
        defer llvm.LLVMDisposeModule(self.module);
        defer llvm.LLVMDisposeBuilder(self.builder);

        self.dispatchArray(self.nodes);

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
