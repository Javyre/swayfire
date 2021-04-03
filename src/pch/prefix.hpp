#include <algorithm>
#include <functional>
#include <iterator>
#include <memory>
#include <optional>
#include <stdexcept>
#include <utility>
#include <vector>

#include <bits/stdint-intn.h>
#include <bits/stdint-uintn.h>
#include <sys/types.h>

#include <wayfire/config/types.hpp>
#include <wayfire/core.hpp>
#include <wayfire/geometry.hpp>
#include <wayfire/nonstd/noncopyable.hpp>
#include <wayfire/nonstd/observer_ptr.h>
#include <wayfire/nonstd/wlroots-full.hpp>
#include <wayfire/object.hpp>
#include <wayfire/option-wrapper.hpp>
#include <wayfire/output.hpp>
#include <wayfire/plugin.hpp>
#include <wayfire/signal-definitions.hpp>
#include <wayfire/util/log.hpp>
#include <wayfire/view-transform.hpp>
#include <wayfire/workspace-manager.hpp>
