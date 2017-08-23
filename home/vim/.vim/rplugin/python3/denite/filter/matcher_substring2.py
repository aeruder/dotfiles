# ============================================================================
# FILE: matcher_substring2.py
# AUTHOR: Andrew Ruder <andy at aeruder.net>
# License: MIT license
# ============================================================================

import re
from .base import Base
from denite.util import split_input


class Filter(Base):
    def __init__(self, vim):
        super().__init__(vim)

        self.name = 'matcher_substring2'
        self.description = 'simple substring matcher'

    def filter(self, context):
        candidates = context['candidates']
        ignorecase = context['ignorecase']
        if context['input'] == '':
            return candidates

        pattern = context['input']
        if ignorecase:
            pattern = pattern.lower()
            candidates = [x for x in candidates
                          if pattern in (x['abbr'] if 'abbr' in x else x['word']).lower()]
        else:
            candidates = [x for x in candidates if pattern in (x['abbr'] if 'abbr' in x else x['word'])]
        return candidates

    def convert_pattern(self, input_str):
        return '\|'.join([re.escape(x) for x in split_input(input_str)])
