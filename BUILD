load("@io_bazel_rules_kotlin//kotlin:kotlin.bzl", "kt_jvm_library", "kt_jvm_binary")
load("@rules_java//java:defs.bzl", "java_binary")

cc_binary(
  name = "envelopes_cc",
  srcs = ["envelopes.cc"],
)

java_binary(
  name = "envelopes_java",
  srcs = ["Envelopes.java"],
  main_class = "Envelopes"
)

kt_jvm_library(
    name = "envelopes_kt_lib",
    srcs = ["Envelopes.kt"],
)

java_binary(
  name = "envelopes_kt",
  runtime_deps = [":envelopes_kt_lib"],
  main_class = "EnvelopesKt"
)