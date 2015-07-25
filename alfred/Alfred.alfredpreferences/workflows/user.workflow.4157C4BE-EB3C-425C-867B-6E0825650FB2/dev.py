#!/usr/bin/python
# encoding: utf-8
#
# Copyright © 2014 stephen.margheim@gmail.com
#
# MIT Licence. See http://opensource.org/licenses/MIT
#
# Created on 11-07-2014
#
from __future__ import unicode_literals
# Standard Library
import re
import sys
import os.path
import subprocess
import utils
from workflow import Workflow, web
from pandoctor import Pandoc



def main(wf):
    """testing area.
    """
    input_path = '/Users/smargheim/Documents/DEVELOPMENT/GitHub/pandoc-templates/examples/academic_test.txt'
    input_name = os.path.splitext(input_path)[0]
    input_dir = os.path.dirname(input_path)
    args = [
            "--bibliography={input_name}.bib", 
            "--csl=/Users/smargheim/Documents/GitHub/pandoc-templates/csl/chicago-author-date.csl",
            "--reference-docx=/Users/smargheim/.pandoc/reference.docx", 
            "{input_file}", 
            "--output={input_name}.docx"
        ]
    print input_path
    print input_dir
    print input_name
    for i, arg in enumerate(args):
        # Replace any and all variables with correct data
        arg = arg.replace('{input_file}', input_path)
        arg = arg.replace('{input_name}', input_name)
        arg = arg.replace('{input_dir}', input_dir)
        args[i] = arg

    print args
    
        

    

if __name__ == '__main__':
    WF = Workflow(libraries=[os.path.join(os.path.dirname(__file__), 'lib')])
    sys.exit(WF.run(main))
