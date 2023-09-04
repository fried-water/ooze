#include "pch.h"

#include "io.h"
#include "ooze/env.h"

namespace ooze {

namespace {

[[noreturn]] void dump_and_exit(const std::vector<std::string>& errors) {
  for(const std::string& line : errors) {
    fmt::print("{}\n", line);
  }

  exit(1);
}

} // namespace

Env create_primative_env() {
  Env env;

  add_tieable_type<bool>(env, "bool");

  add_tieable_type<i8>(env, "i8");
  add_tieable_type<i16>(env, "i16");
  add_tieable_type<i32>(env, "i32");
  add_tieable_type<i64>(env, "i64");

  add_tieable_type<u8>(env, "u8");
  add_tieable_type<u16>(env, "u16");
  add_tieable_type<u32>(env, "u32");
  add_tieable_type<u64>(env, "u64");

  add_tieable_type<f32>(env, "f32");
  add_tieable_type<f64>(env, "f64");

  add_tieable_type<std::string>(env, "string");

  env.add_type<std::vector<std::byte>>("vector<byte>");

  env.add_function("write", [](const std::string& file, const std::vector<std::byte>& bytes) {
    const auto r = write_binary_file(file, bytes);
    if(!r) dump_and_exit(r.error());
  });

  env.add_function("write", [](const std::string& file, const std::string& bytes) {
    const auto r =
      write_binary_file(file, Span<std::byte>{reinterpret_cast<const std::byte*>(bytes.data()), bytes.size()});

    if(!r) dump_and_exit(r.error());
  });

  env.add_function("read", [](const std::string& file) {
    if(auto r = read_binary_file(file); r) {
      return std::move(*r);
    } else {
      dump_and_exit(r.error());
    }
  });

  env.add_function("read", [](const std::string& file) {
    if(auto r = read_text_file(file); r) {
      return std::move(*r);
    } else {
      dump_and_exit(r.error());
    }
  });

  return env;
}

} // namespace ooze
