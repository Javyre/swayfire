#include <iterator>
#include <stdexcept>

namespace nonstd {

/// Skip first adaptor.
template <class T> struct AdaptSkipFirst {
    T &inner;

    auto begin() {
        auto b = std::begin(inner);
        if (b == std::end(inner))
            throw std::range_error("Cannot skip first element of empty range.");

        return b + 1;
    }

    auto end() { return std::end(inner); }
};

/// Skip last adaptor.
template <class T> struct AdaptSkipLast {
    T &inner;

    auto begin() { return std::begin(inner); }

    auto end() {
        auto e = std::end(inner);
        if (e == std::begin(inner))
            throw std::range_error("Cannot skip last element of empty range.");

        return e - 1;
    }
};

/// Reverse iteration adaptor.
template <class T> struct AdaptReverse {
    T &inner;

    auto begin() { return std::rbegin(inner); }
    auto end() { return std::rend(inner); }
};

#define DECL_ADAPTOR_PIPE(ADAPTER, PIPE_NAME)                                  \
    struct ADAPTER##Fwd {};                                                    \
    constexpr ADAPTER##Fwd PIPE_NAME = {};                                     \
                                                                               \
    template <class T> inline ADAPTER<T> operator|(T &inner, ADAPTER##Fwd) {   \
        return {inner};                                                        \
    }

DECL_ADAPTOR_PIPE(AdaptSkipFirst, skip_first);
DECL_ADAPTOR_PIPE(AdaptSkipLast, skip_last);
DECL_ADAPTOR_PIPE(AdaptReverse, reverse);

} // namespace nonstd
