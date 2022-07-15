#include "pch.h"

#include "ooze/env.h"

namespace ooze {

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

  return env;
}

} // namespace ooze
