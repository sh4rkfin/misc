class MultiDimArray:
    def __init__(self, values, *dims):
        self.dim_count = len(dims)
        self.dims = dims
        self.values = values

    def __len__(self):
        return self.dims[0]

    def __iter__(self):
        for x in range(self.dims[0]):
            yield self[x]

    def __getitem__(self, coord):
        if coord in self.values:
            value = self.values[coord]
            if self.dim_count == 1:
                return value
            return MultiDimArray(value, *self.dims[1:])
        if self.dim_count == 1:
            return 0
        return EmptyMultiDimArray.get_instance(self.dims[1:])

    def __setitem__(self, coord):
        raise Exception("Instances of {0} cannot be set".format(self.__class__.__name__))


class EmptyMultiDimArray(MultiDimArray):
    instances = {}

    @staticmethod
    def get_instance(dimensions):
        if len(dimensions) < 3:
            if dimensions not in EmptyMultiDimArray.instances:
                EmptyMultiDimArray.instances[dimensions] = EmptyMultiDimArray(*dimensions)
            return EmptyMultiDimArray.instances[dimensions]
        return EmptyMultiDimArray(*dimensions)

    def __init__(self, *dims):
        MultiDimArray.__init__(self, None, *dims)

    def __getitem__(self, _):
        if self.dim_count > 1:
            return EmptyMultiDimArray.get_instance(self.dims[1:])
        return 0
