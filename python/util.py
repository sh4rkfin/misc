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