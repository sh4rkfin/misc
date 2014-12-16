#!/usr/bin/env python
import unittest
import string


class RebalanceTest(unittest.TestCase):
    def test_template(self):
        template = string.Template("$who likes $what")
        m = dict(who="dave")
        self.assertRaises(KeyError, template.substitute, m)
        m['what'] = 'bagels'
        self.assertEquals("dave likes bagels", template.substitute(m))
        m['when'] = 'now'
        self.assertEquals("dave likes bagels", template.substitute(m))
        print "sub: ", template.substitute(m)

    def test_template_param_name(self):
        template = string.Template("$which_person likes $what")
        m = dict(which_person="dave")
        self.assertRaises(KeyError, template.substitute, m)
        m['what'] = 'bagels'
        self.assertEquals("dave likes bagels", template.substitute(m))
        m['when'] = 'now'
        self.assertEquals("dave likes bagels", template.substitute(m))
        print "sub: ", template.substitute(m)

    def test_template_type(self):
        s = "$which_person likes $what"
        template = string.Template(s)
        self.assertTrue(type(s) is str)
        print "type string: ", type(s), type(s) is str
        print "type template: ", type(template), type(template) is string.Template


if __name__ == '__main__':
    unittest.main()
