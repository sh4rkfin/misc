import util
from subprocess import call
from string import Template
import os

SOLVER = "/Users/dfinlay/eclipse-projects/glpk-4.35/examples/glpsol"


def invoke_solver(model, data_file_name, result_file_name, options=None):
    cmd = [SOLVER,
           "-m", model,
           "-d", data_file_name,
           "-o", result_file_name]
    if options is not None:
        cmd.extend(options)
    return call(cmd)


def read_solution_status(result_file_name):
    status = read_status(result_file_name)
    if status[1] != 'OPTIMAL':
        return status
    return 0


def glpsol_solve(model, data_file_name, result_file_name, options=None):
    result = invoke_solver(model, data_file_name, result_file_name, options)
    if result != 0:
        return result
    return read_solution_status(result_file_name)


def make_data_file_name(working_dir, data_file_name_template, params):
    if not working_dir:
        working_dir = "."
    return working_dir + "/" + data_file_name_template.substitute(params)


def make_result_file_name(working_dir, result_file_name_template, params):
    if not working_dir:
        working_dir = "."
    return working_dir + "/" + result_file_name_template.substitute(params)


def solve(working_dir,
          model,
          data_file_name_template,
          data_file_template,
          result_file_name_template,
          params,
          options=None,
          use_existing_solution=False):
    result_file = make_result_file_name(working_dir, result_file_name_template, params)
    if use_existing_solution and os.path.isfile(result_file):
        print "found existing solution in file: ", result_file
        return read_solution_status(result_file)

    data_file_name = make_data_file_name(working_dir, data_file_name_template, params)
    with open(data_file_name, "w") as text_file:
        text_file.write(data_file_template.substitute(params))
    return glpsol_solve(model, data_file_name, result_file, options)


def read_status(file_name):
    result = []

    def status_shredder(matcher):
        result.append(matcher.group(1))
        result.append(matcher.group(2))
    util.parse(file_name, "Status:\s*(\w+)\s*(\w+)", status_shredder)
    return result


def read_variable(filename, var_name):
    result = []
    # groups for the following regular expression
    #             (g1) (g2(g3)(g4(g5))        (g6)    (g7 (g8))
    regex = "{0}\[(\d+)(,(\d+)(,(\d+))?)?\]\s*([A-Z\*]+)?\s*(\d+(\.\d+)?)".format(var_name)

    def my_proc(m):
        i = int(m.group(1))
        j = int(m.group(3)) if m.group(3) is not None else None
        k = int(m.group(5)) if m.group(5) is not None else None
        val = float(m.group(7))
        if j is not None:
            util.ensure_has_capacity(result, i + 1, lambda: [])
            if k is not None:
                util.ensure_has_capacity(result[i], j + 1, lambda: [])
                util.ensure_has_capacity(result[i][j], k + 1)
                result[i][j][k] = val
            else:
                util.ensure_has_capacity(result[i], j + 1)
                result[i][j] = val
        else:
            util.ensure_has_capacity(result, i + 1)
            result[i] = val

    util.parse(filename, regex, my_proc)
    return result


def read_2d_variable_as_map(filename, var_name):
    result = {}
    regex = "{0}\[(\d+),(\d+)\]\s*(\*)?\s*(\d+(\.\d+)?)".format(var_name)

    def my_proc(m):
        value = float(m.group(4))
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
        raise TypeError("not supported for this type")


class ModelInstance:
    def __init__(self, model, params, data_file_name_template, result_file_name_template, working_dir):
        self.model = model
        self.params = params
        self.data_file_name_template = as_template(data_file_name_template)
        self.result_file_name_template = as_template(result_file_name_template)
        self.working_dir = working_dir
        self.variables = {'array': {}, 'map': {}}
        self._use_existing_solution = False

    def get_data_file_name(self):
        return self.working_dir + "/" + self.data_file_name_template.substitute(self.params)

    def get_model_name(self):
        return self.model.model_name

    def set_use_existing_solution(self, value):
        self._use_existing_solution = value

    def set_params(self, params):
        self.params = params

    def get_result_file(self):
        return self.working_dir + "/" + self.result_file_name_template.substitute(self.params)

    def solve(self, params=None, options=None):
        if params is not None:
            # just use the previously set params
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
                     self.params,
                     options,
                     self._use_existing_solution)

    def get_variable_cache(self, name):
        return self.variables[name]

    def get_variable(self, var_name):
        vars = self.get_variable_cache('array')
        if var_name not in vars:
            var = read_variable(self.get_result_file(), var_name)
            vars[var_name] = var
        return vars[var_name]

    def get_2d_variable_as_map(self, var_name):
        vars = self.get_variable_cache('map')
        if var_name not in vars:
            var = read_2d_variable_as_map(self.get_result_file(), var_name)
            vars[var_name] = var
        return vars[var_name]





