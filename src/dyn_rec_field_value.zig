const std = @import("std");

const builtin = std.builtin;
const math = std.math;
const mem = std.mem;
const meta = std.meta;

const TypeInfo = builtin.TypeInfo;

const sort = std.sort.sort;

const RecursiveField = @import("recursive_field.zig").RecursiveField;

pub fn DynRecFieldValue(comptime T: type) type {
    comptime var fields = RecursiveField.of(T);
    sort(RecursiveField, fields, {}, RecursiveField.lessThan);

    comptime var collapsed_fields: [fields.len]RecursiveField = undefined;

    var write = 0;
    for (fields) |field, i| {
        if (i == 0 or field.field_type != fields[i - 1].field_type) {
            collapsed_fields[write] = field;
            write += 1;
        }
    }

    const num_types = if (fields.len == 0) 0 else write;

    var _tag_fields: [num_types]TypeInfo.EnumField = undefined;
    var _union_fields: [num_types]TypeInfo.UnionField = undefined;
    var _types: [num_types]type = undefined;
    for (collapsed_fields) |field, i| {
        if (i >= num_types) break;
        const type_name = @typeName(field.field_type); // TODO: mangle this better
        _tag_fields[i] = TypeInfo.EnumField{
            .name = type_name,
            .value = i,
        };
        _union_fields[i] = TypeInfo.UnionField{
            .name = type_name,
            .field_type = field.field_type,
            .alignment = @alignOf(field.field_type),
        };
        _types[i] = field.field_type;
    }

    const tag_fields = &_tag_fields;
    const union_fields = &_union_fields;
    const __types = _types;

    const TagType = @Type(.{ .Enum = .{
        .layout = .Auto,
        .tag_type = math.IntFittingRange(0, num_types - 1),
        .fields = tag_fields,
        .decls = &[0]TypeInfo.Declaration{},
        .is_exhaustive = true,
    } });

    const UnionType = @Type(.{ .Union = .{
        .layout = .Extern,
        .tag_type = null,
        .fields = union_fields,
        .decls = &[0]TypeInfo.Declaration{},
    } });

    return struct {
        tag: TagType,
        value: UnionType,

        const Self = @This();

        pub const types = __types;

        pub const DynType = TagType;

        pub fn of(comptime Type: type, val: Type) Self {
            comptime var tag = dynType(Type) orelse @compileError("Type is not a tag of union");
            var value: UnionType = undefined;
            @ptrCast(*Type, &value).* = val;
            return .{ .tag = tag, .value = value };
        }

        pub fn dynType(comptime Ty: type) ?TagType {
            for (types) |ty, i| {
                if (ty == Ty) return @intToEnum(TagType, tag_fields[i].value);
            }
            return null;
        }

        pub fn eq(self: Self, other: Self) bool {
            if (self.tag != other.tag) return false;

            inline for (union_fields) |field| {
                if (mem.eql(u8, @tagName(self.tag), field.name)) {
                    var self_val = @field(self.value, field.name);
                    var other_val = @field(other.value, field.name);
                    return meta.eql(self_val, other_val);
                }
            }

            unreachable;
        }

        pub fn fromRaw(dyn_type: TagType, data: [*]const u8, bit_offset: u3) Self {
            var out: Self = .{ .tag = dyn_type, .value = undefined };

            copyAndBitshiftDown(data, mem.asBytes(&out.value), bit_offset);

            return out;
        }

        pub fn fromZst(dyn_type: TagType) Self {
            // Since the type_table is not in this struct, this cannot be safety
            // checked.
            return .{ .tag = dyn_type, .value = undefined };
        }
    };
}

fn copyAndBitshiftDown(source: [*]const u8, dest: []u8, bit_offset: u3) void {
    for (dest) |*out, i| {
        var int = mem.readIntLittle(u16, @ptrCast(*const [2]u8, &source[i]));
        out.* = @truncate(u8, int >> bit_offset);
    }
}
