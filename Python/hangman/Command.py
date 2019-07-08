# -*- coding: utf-8 -*-
# Copyright (C) 2018, Christopher Hume.  All rights reserved.
#
# You should have received a copy of the MIT License along with this program.
# If not, see https://opensource.org/licenses/MIT.
#
# 2018-07-11 CNHume Created Command class
#
import sys

class Command(object):
  """Command Class"""
  UTF8 = u'utf-8'
  VERSION = 1.0

  def __init__(self, word_file, art_file, file_ext, trials):
    self.trials = trials
    self.verbose = False                # -v emit debugging information
    self.art_file = art_file
    self.word_file = word_file
    self.file_ext = file_ext

  @staticmethod
  def arg_msg(s, name=u'argument'):
    msg = u'Invalid '
    msg += name
    msg += u': '
    msg += s
    return msg

  def Parse(self, argv):
    """Command Line Parser"""
    argc = len(argv)
    usage = False
    n = 0
    script_name = argv[n]
    n += 1
    while n < argc and not usage:       # parse any switches
      token = argv[n]
      token_len = len(token)
      if token_len < 1 or token[0] != '-':
        break                           # end of switches
      elif token_len < 2:
        usage = True
      else:                             # parse single character switch
        if token[1] == 't':             # the trials switch
          if token_len > 2:             # whitespace optional
            trials = token[2:]
          elif n < argc:                # whitespace allowed
            n += 1
            trials = argv[n]
          self.trials = int(trials)
        elif token[1] == 'v':           # the verbose switch
          if token_len > 2:             # superfluous value specified
            usage = True
          else:
            self.verbose = True
        elif token[1] == 'x':           # the file_ext switch
          if token_len > 2:             # whitespace optional
            self.file_ext = token[2:]
          elif n < argc:                # whitespace allowed
            n += 1
            self.file_ext = argv[n]
        elif token[1] == '?':           # the help switch
          usage = True
        else:                           # switch unknown
          usage = True
      n += 1

    if n < argc:                        # parse word_file to be searched
      self.word_file = argv[n].decode(Command.UTF8)
      n += 1

    if n < argc:                        # parse art_path
      self.art_file = argv[n].decode(Command.UTF8)
      n += 1                            # art_path is optional

    if n < argc:                        # superfluous argument specified
      usage = True

    if self.verbose:
      self.Log()

    if usage:                           # throw usage line if parse failed
      print(u'Usage: python {0} [-t trials] [-v] [-x file_ext] [word_file [art_file]]'\
        .format(script_name))

    return not usage

  def Log(self):
    print(u'{0}: {1}'.format(u'word_file', self.word_file))
    print(u'{0}: {1}'.format(u'art_file', self.art_file))
    print(u'{0}: {1}'.format(u'file_ext', self.file_ext))
    print(u'{0}: {1}'.format(u'verbose', self.verbose))
