import re


def parse(filename, regex, processor):
    with open(filename, "r") as f:
        matcher = re.compile(regex)
        for line in f:
            m = matcher.search(line)
            if m:
                processor(m)


def ensure_has_capacity(array, size, default_value_function=None):
    current_size = len(array)
    if size > current_size:
        for _ in range(size - current_size):
            val = default_value_function() if default_value_function else 0
            array.append(val)


def is_iterable(obj):
    return hasattr(obj, '__iter__') or \
        hasattr(obj, '__len__') and hasattr(obj, '__getitem__')


def sum_off_diagonal(twod_array):
    result = 0
    for i, x in enumerate(twod_array):
        for j, y in enumerate(x):
            if i != j:
                result += y
    return result


def dimension_count(m):
    current = m
    result = 0
    while isinstance(current, list):
        result += 1
        if len(current) == 0:
            break
        current = current[0]
    return result


def add_to(m1, m2):
    if m1 is None:
        m1 = make_zero_matrix_of_same_dimension(m2)
    if m2 is None:
        return m1
    l1, l2 = len(m1), len(m2)
    if l1 > l2:
        raise ValueError("lengths not compatible")
    is_list = None
    for i in xrange(l1):
        if is_list is None:
            is_list = isinstance(m1[i], list)
        if is_list:
            add_to(m1[i], m2[i])
        else:
            m1[i] += m2[i]
    return m1


def make_zero_matrix_of_same_dimension(m):
    return [make_zero_matrix_of_same_dimension(v) for v in m] if isinstance(m, list) else 0


def accumulate(collection, accumulator, initial_value=None):
    result = initial_value
    for x in collection:
        result = accumulator(result, x)
    return result


def arg_max(lst, key):
    max_val, result = None, None
    for a in lst:
        if max_val is None or max_val < key(a):
            max_val, result = key(a), a
    return result


def eq(x, y, tol):
    return abs(x - y) < tol


def lt(x, y, tol):
    return x < y and not eq(x, y, tol)


def le(x, y, tol):
    return x <= y or eq(x, y, tol)


def ge(x, y, tol):
    return x >= y or eq(x, y, tol)


def minimize(lst, value_getter):
    result = None
    for a in lst:
        value = value_getter(a)
        if result is None or result > value:
            result = value
    return result
