PROJECT = task_monitor
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

DEPS = chronos
deps_chronos = git https://github.com/lehoff/chronos.git
LOCAL_DEPS = crypto
include erlang.mk
