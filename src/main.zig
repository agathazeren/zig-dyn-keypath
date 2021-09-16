const std = @import("std");
const mem = std.mem;
const meta = std.meta;
const testing = std.testing;
const builtin = std.builtin;
const debug = std.debug;
const math = std.math;

const sort = std.sort.sort;

const TypeInfo = builtin.TypeInfo;

const ComptimeStringMap = std.ComptimeStringMap;

const RecursiveField = @import("recursive_field.zig").RecursiveField;
pub const DynRecFieldValue = @import("dyn_rec_field_value.zig").DynRecFieldValue;

//pub fn Keypath(T: type) type {}

//pub fn DynFieldType(T: type) type {}

pub fn RecursiveKeypath(comptime T: type) type {
    const fields = RecursiveField.of(T);

    var builder = KeypathBuilder(@bitSizeOf(T)){};

    for (fields) |field| {
        builder.add(recursiveBitOffset(T, field.path), @bitSizeOf(field.field_type) == 0);
    }
    builder.done = true;

    const tagDefn = builder.tagDefn();

    var _enum_fields: [fields.len]TypeInfo.EnumField = undefined;

    for (fields) |field, i| {
        _enum_fields[i] = TypeInfo.EnumField{
            .name = mangle(field.path),
            .value = builder.tag(recursiveBitOffset(T, field.path), @bitSizeOf(field.field_type) == 0),
        };
    }

    comptime var _names: [fields.len - 1][]const u8 = undefined;
    for (fields) |field, i| {
        if (field.path.len == 0) continue;
        _names[i - 1] = field.path[field.path.len - 1];
    }
    sort([]const u8, &_names, {}, lexicalSort);
    comptime var _name_count = 0;
    for (_names) |name, i| {
        if (i == 0 or !mem.eql(u8, name, _names[i - 1]))
            _name_count += 1;
    }
    comptime var _names_dedup: [_name_count][]const u8 = undefined;
    comptime var _names_j = 0;
    for (_names) |name, i| {
        if (i == 0 or !mem.eql(u8, name, _names[i - 1])) {
            _names_dedup[_names_j] = name;
            _names_j += 1;
        }
    }

    const Inner = @Type(.{ .Enum = TypeInfo.Enum{
        .layout = .Auto,
        .tag_type = tagDefn.TagType(),
        .fields = &_enum_fields,
        .decls = &[0]TypeInfo.Declaration{},
        .is_exhaustive = true,
    } });

    const enum_fields = _enum_fields;
    const name_count = _name_count;
    const names = _names_dedup;

    return struct {
        _inner: Inner,

        const Self = @This();

        pub fn root() Self {
            return .{ ._inner = @field(Inner, "__keypath") }; // mangle(&[0][]const u8{}));
        }

        pub fn key(self: Self, name: []const u8) ?Self {
            if (name_id_map.get(name)) |name_id| {
                return subkey_table[@enumToInt(self._inner)][name_id];
            } else {
                return null;
            }
        }

        //pub fn up(self: Self) ?Self {}

        pub fn get(self: Self, obj: *const T) DynRecFieldValue(T) {
            if (self.isZst()) {
                return DynRecFieldValue(T).fromZst(self.dynType());
            } else {
                return DynRecFieldValue(T).fromRaw(
                    self.dynType(),
                    @ptrCast([*]const u8, &@ptrCast([*]const u8, obj)[self.byteOffset().?]),
                    @intCast(u3, self.bitOffset().? - self.byteOffset().? * 8),
                );
            }
        }

        //pub fn getDuck(self: Self, Duck: type, obj: Duck) DynRecFieldValue(Duck) {}

        //pub fn set(self: Self, obj: *T, value: DynRecFieldValue(T)) void {}

        //pub fn setDuck(self: Self, Duck: type, obj: *Duck, value: DynRecFieldValue(T)) void {}

        const Int = meta.TagType(Inner);

        fn toInt(self: Self) Int {
            return @enumToInt(self._inner);
        }

        const DynType = DynRecFieldValue(T).DynType;

        const table_size = math.maxInt(Int) + 1;

        const type_table = tbl: {
            var table: [table_size]DynType = undefined;
            for (fields) |field, i| {
                table[enum_fields[i].value] = DynRecFieldValue(T).dynType(field.field_type).?;
            }
            break :tbl table;
        };
        const NameId = math.IntFittingRange(0, name_count);
        const name_id_map: type = ComptimeStringMap(NameId, kvs: {
            comptime var _kvs: [name_count]struct { @"0": []const u8, @"1": NameId } = undefined;
            for (names) |name, i| {
                _kvs[i] = .{ .@"0" = name, .@"1" = i };
            }
            break :kvs _kvs;
        });

        const subkey_table: [table_size][math.maxInt(NameId) + 1]?Self = tbl: {
            var table: [table_size][math.maxInt(NameId) + 1]?Self = undefined;
            for (fields) |field, i| {
                for (table[i]) |*k| {
                    k.* = null;
                }
                switch (meta.activeTag(@typeInfo(field.field_type))) {
                    // Note that non-extern unions cannot be included, becase we won't set the tag right.
                    .Struct => {},
                    .Union => if (@typeInfo(field.field_type).Union.layout != .Extern) continue,
                    else => continue,
                }
                for (meta.fields(field.field_type)) |subfield| {
                    var name_id = name_id_map.get(subfield.name).?;
                    var subfield_path = field.path ++ [1][]const u8{subfield.name};
                    for (fields) |upcoming_field, upcoming_i| {
                        if (eqlStringSlices(upcoming_field.path, subfield_path)) {
                            table[i][@as(usize, name_id)] = .{
                                ._inner = @intToEnum(Inner, enum_fields[upcoming_i].value),
                            };
                            break;
                        }
                    }
                }
                //                @compileLog("i: ", i, "field: ", field.path, "subs", table[i]);
            }
            break :tbl table;
        };

        fn dynType(self: Self) DynType {
            return type_table[@enumToInt(self._inner)];
        }

        fn isZst(self: Self) bool {
            return tagDefn.isZst(self.toInt());
        }

        fn bitOffset(self: Self) ?usize {
            return tagDefn.bitOffset(self.toInt());
        }

        fn byteOffset(self: Self) ?usize {
            if (self.bitOffset()) |bit_offset| {
                // No div by zero or overflow possible.
                return math.divFloor(usize, bit_offset, 8) catch unreachable;
            } else {
                return null;
            }
        }
    };
}

fn lexicalSort(_: void, lhs: []const u8, rhs: []const u8) bool {
    return mem.lessThan(u8, lhs, rhs);
}

fn recursiveBitOffset(comptime T: type, path: []const []const u8) comptime_int {
    if (path.len == 0) return 0;
    return @bitOffsetOf(T, path[0]) + recursiveBitOffset(
        meta.fieldInfo(T, meta.stringToEnum(meta.FieldEnum(T), path[0]).?).field_type,
        path[1..],
    );
}

fn LeadingBits(comptime T: type) type {
    if (T == comptime_int) {
        return ?comptime_int;
    } else {
        return ?math.IntFittingRange(0, meta.bitCount(T));
    }
}

fn leadingBits(bit: u1, val: anytype) LeadingBits(@TypeOf(val)) {
    if (val == 0) return null;
    var leading: LeadingBits(@TypeOf(val)) = 0;
    var _val = val;
    while (_val & 0b1 == bit) {
        leading.? += 1;
        _val >>= 1;
    }
    return leading;
}

fn mangle(comptime path: []const []const u8) []const u8 {
    // TODO: escape dots so that struct {@"a.__kp_b": usize, a: struct {b: usize}}
    // doesn't create an ambigous name.

    const sep = ".__kp_";

    comptime var name: []const u8 = "__keypath";

    comptime {
        for (path) |component| {
            name = name ++ sep ++ component;
        }
    }

    return name;
}

fn KeypathBuilder(comptime bit_size: comptime_int) type {
    return struct {
        fields: [bit_size]comptime_int = [1]comptime_int{0} ** bit_size,
        zst_count: comptime_int = 0,

        max_count: comptime_int = 0,
        min_bitalign: comptime_int = math.maxInt(usize), // suitably large to effectivly be infinite
        max_bit_offset: comptime_int = 0,

        done: bool = false,

        next_id: [bit_size]comptime_int = [1]comptime_int{0} ** bit_size,
        next_zst: comptime_int = 0,

        const Self = @This();

        fn add(comptime self: *Self, comptime bit_offset: comptime_int, is_zst: bool) void {
            debug.assert(!self.done);
            if (is_zst) {
                self.zst_count += 1;
                return;
            }

            var bitalign = leadingBits(0, bit_offset);
            if (bitalign == null) {
                bitalign = math.maxInt(usize); // suitably large to effectly infinite
            }
            if (bitalign.? < self.min_bitalign) self.min_bitalign = bitalign.?;

            if (bit_offset > self.max_bit_offset) self.max_bit_offset = bit_offset;

            self.fields[bit_offset] += 1;
            if (self.fields[bit_offset] > self.max_count) self.max_count = self.fields[bit_offset];
        }

        fn tagDefn(comptime self: *Self) TagDefn {
            debug.assert(self.done == true);

            return TagDefn{
                .min_bitalign = self.min_bitalign,
                .max_bit_offset = self.max_bit_offset,
                .max_disambig = self.max_count - 1,
                .num_zst = self.zst_count,
            };
        }

        fn tag(comptime self: *Self, bit_offset: comptime_int, is_zst: bool) comptime_int {
            debug.assert(self.done);

            var defn = self.tagDefn();

            if (is_zst) {
                var _tag = defn.maxSizedTag() + self.next_zst;
                self.next_zst += 1;
                return _tag;
            } else {
                var out: defn.TagType() = 0;
                out |= bit_offset >> defn.min_bitalign;
                var id = self.next_id[bit_offset];
                if (id > 0) {
                    out |= id << defn.offsetSize();
                }
                self.next_id[bit_offset] += 1;
                return out;
            }
        }
    };
}

const TagDefn = struct {
    min_bitalign: comptime_int,
    max_bit_offset: comptime_int,
    max_disambig: comptime_int,
    num_zst: comptime_int,

    pub fn TagType(comptime self: *const @This()) type {
        return math.IntFittingRange(0, self.maxSizedTag() + self.num_zst);
    }

    pub fn maxSizedTag(comptime self: *const TagDefn) comptime_int {
        comptime var max_int = 0;
        max_int = 1 << self.offsetSize();
        // The + 1 ensures that a max_disambig of 0 will give a shift of zero,
        // and 1 or more will give a shift of 1 or more.
        max_int <<= math.log2_int_ceil(usize, @as(usize, self.max_disambig + 1));
        max_int -= 1;
        if (max_int <= 0) max_int = 1; // so that the root is different than the
        // first zst field in a struct of all
        // zsts
        return max_int;
    }

    pub fn offsetSize(comptime self: *const TagDefn) comptime_int {
        if (self.max_bit_offset == 0) return 0;
        const size = math.log2_int_ceil(usize, @as(usize, self.max_bit_offset)) - self.min_bitalign;
        return size;
    }

    pub fn isZst(comptime self: *const TagDefn, tag: self.TagType()) bool {
        return tag > self.maxSizedTag();
    }

    pub fn bitOffset(comptime self: *const TagDefn, tag: self.TagType()) ?usize {
        if (self.isZst(tag)) return null;
        var dat = @as(usize, tag);
        dat &= (1 << self.offsetSize()) - 1;
        dat <<= self.min_bitalign;
        return dat;
    }
};

fn eqlStringSlices(lhs: []const []const u8, rhs: []const []const u8) bool {
    if (lhs.len != rhs.len) return false;
    for (lhs) |lhs_str, idx| {
        const rhs_str = rhs[idx];
        if (lhs_str.len != rhs_str.len) return false;
        for (lhs_str) |lhs_byte, str_idx| {
            const rhs_byte = rhs_str[str_idx];
            if (lhs_byte != rhs_byte) return false;
        }
    }
    return true;
}

test "mangling" {
    try testing.expectEqual(mangle(&.{ "a", "b", "c" }), "__keypath.__kp_a.__kp_b.__kp_c");
}

test "keypaths" {
    const B = struct {
        j: usize,
    };

    const Z = struct {
        a: usize,
        b: B,
        c: usize,
    };

    const T = struct {
        x: usize,
        y: usize,
        z: Z,
        zz: Z,
    };

    var t = T{
        .x = 1,
        .y = 2,
        .z = Z{
            .a = 3,
            .b = B{ .j = 4 },
            .c = 5,
        },
        .zz = Z{
            .a = 10,
            .b = B{ .j = 40 },
            .c = 42,
        },
    };

    var kp = RecursiveKeypath(T).root();

    var x_kp = kp.key("x").?;

    try testing.expect(x_kp.get(&t).eq(DynRecFieldValue(T).of(usize, 1)));
}
