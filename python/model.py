import util
from subprocess import call
from string import Template

SOLVER = "/Users/dfinlay/eclipse-projects/glpk-4.35/examples/glpsol"

def glpsol_solve(model, data_file_name, result_file_name):
    result = call([SOLVER,
                   "-m", model,
                   "-d", data_file_name,
                   "-o", result_file_name])
    if result != 0:
        return result
    status = read_status(result_file_name)
    if status[1] != 'OPTIMAL':
        return status
    return 0

def solve(working_dir, model, data_file_name_template, data_file_template, result_file_name_template, params):
    if not working_dir:
        working_dir = "."

    cluster_file_name = working_dir + "/" + data_file_name_template.substitute(params)

    with open(cluster_file_name, "w") as text_file:
        text_file.write(data_file_template.substitute(params))

    result_file = working_dir + "/" + result_file_name_template.substitute(params)
    return glpsol_solve(model, cluster_file_name, result_file)


def read_status(file_name):
    result = []

    def status_shredder(matcher):
        result.append(matcher.group(1))
        result.append(matcher.group(2))
    util.parse(file_name, "Status:\s*(\w+)\s*(\w+)", status_shredder)
    return result

def read_1d_variable(filename, var_name):
    result = []
    regex = "{0}\[(\d+)\]\s*(\*)?\s*(\d+)".format(var_name)

    def my_proc(m):
        row = int(m.group(1))
        util.ensure_has_capacity(result, row + 1)
        result[row] = int(m.group(3))
    util.parse(filename, regex, my_proc)
    return result


def read_2d_variable(filename, var_name):
    result = []
    regex = "{0}\[(\d+),(\d+)\]\s*(\*)?\s*(\d+)".format(var_name)

    def my_proc(m):
        row = int(m.group(1))
        util.ensure_has_capacity(result, row + 1, lambda: [])
        array = result[row]
        col = int(m.group(2))
        util.ensure_has_capacity(array, col + 1)
        result[row][col] = int(m.group(4))
    util.parse(filename, regex, my_proc)
    return result


def read_3d_variable(filename, var_name):
    result = []
    regex = "{0}\[(\d+),(\d+),(\d+)\]\s*(\*)?\s*(\d+)".format(var_name)

    def my_proc(m):
        i = int(m.group(1))
        util.ensure_has_capacity(result, i + 1, lambda: [])
        j = int(m.group(2))
        util.ensure_has_capacity(result[i], j + 1, lambda: [])
        k = int(m.group(3))
        util.ensure_has_capacity(result[i][j], k + 1)
        result[i][j][k] = int(m.group(5))
    util.parse(filename, regex, my_proc)
    return result


def read_2d_variable_as_map(filename, var_name):
    result = {}
    regex = "{0}\[(\d+),(\d+)\]\s*(\*)?\s*(\d+)".format(var_name)

    def my_proc(m):
        value = int(m.group(4))
        if value == 0:
            return
        row = int(m.group(1))
        col = int(m.group(2))
        if row not in result:
            result[row] = {}
        result[row][col] = value
    util.parse(filename, regex, my_proc)
    return result


def as_template(s):
    if type(s) is Template:
        return s
    return Template(s)


class Model:
    def __init__(self, model_name, data_file_template):
        self.model_name = model_name
        self.data_file_template = as_template(data_file_template)

    def get_name(self):
        return self.model_name

    def get_data_file_template(self):
        return self.data_file_template

    def solve(self, params, data_file_name_template, result_file_name_template, working_dir):
        instance = ModelInstance(self.model,
                                 self.params,
                                 data_file_name_template,
                                 result_file_name_template,
                                 working_dir)
        instance.solve(params)
        return instance


class ModelInstance:
    def __init__(self, model, params, data_file_name_template, result_file_name_template, working_dir):
        self.model = model
        self.params = params
        self.data_file_name_template = as_template(data_file_name_template)
        self.result_file_name_template = as_template(result_file_name_template)
        self.working_dir = working_dir
        self.variables = {'1d': {}, '2d': {}, '2d_map': {}, '3d': {}}

    def get_data_file_name(self):
        return self.working_dir + "/" + self.data_file_name_template.substitute(self.params)

    def get_model_name(self):
        return self.model.model_name

    def get_result_file(self):
        return self.working_dir + "/" + self.result_file_name_template.substitute(self.params)

    def solve(self, params):
        self.params = params
        cluster_file_name = self.get_data_file_name()

        data_file_template = self.model.data_file_template

        with open(cluster_file_name, "w") as text_file:
            text_file.write(data_file_template.substitute(self.params))

        return solve(self.working_dir,
                     self.model.get_name(),
                     self.data_file_name_template,
                     self.model.get_data_file_template(),
                     self.result_file_name_template,
                     self.params)

    def get_variable_cache(self, name):
        return self.variables[name]

    def get_1d_variable(self, var_name):
        vars = self.get_variable_cache('1d')
        if var_name not in vars:
            var = read_1d_variable(self.get_result_file(), var_name)
            vars[var_name] = var
        return vars[var_name]

    def get_2d_variable(self, var_name):
        vars = self.get_variable_cache('2d')
        if var_name not in vars:
            var = read_2d_variable(self.get_result_file(), var_name)
            vars[var_name] = var
        return vars[var_name]

    def get_3d_variable(self, var_name):
        vars = self.get_variable_cache('2d')
        if var_name not in vars:
            var = read_3d_variable(self.get_result_file(), var_name)
            vars[var_name] = var
        return vars[var_name]

    def get_2d_variable_as_map(self, var_name):
        vars = self.get_variable_cache('2d_map')
        if var_name not in vars:
            var = read_2d_variable_as_map(self.get_result_file(), var_name)
            vars[var_name] = var
        return vars[var_name]





