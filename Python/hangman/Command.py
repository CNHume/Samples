# Copyright (C) 2018, Christopher Hume.  All rights reserved.
#
# You should have received a copy of the MIT License along with this program.
# If not, see https://opensource.org/licenses/MIT.
#
# 2018-07-11 CNHume Created Command class
#
VERSION = 1.0


class Command(object):
  """Command Class"""

  def __init__(self, word_file, art_file, file_ext, trials, verbose=False):
    self.trials = trials
    self.art_file = art_file
    self.word_file = word_file
    self.file_ext = file_ext
    # verbose emits debugging output
    self.verbose = verbose

  def Parse(self, argv):
    """Command Line Parser"""
    argc = len(argv)
    usage = False
    n = 0
    script_name = argv[n]
    n += 1
    # parse any switches
    while n < argc and not usage:
      token = argv[n]
      token_len = len(token)
      if token_len < 1 or token[0] != '-':
        # end of switches
        break
      elif token_len < 2:
        # switch character missing
        usage = True
      else:
        # parse single character switch
        if token[1] == 't':
          # the trials switch
          if token_len > 2:
            # whitespace optional
            trials = token[2:]
          elif n < argc:
            # whitespace allowed
            n += 1
            trials = argv[n]
          self.trials = int(trials)
        elif token[1] == 'v':
          # the verbose switch
          if token_len > 2:
            # superfluous value specified
            usage = True
          else:
            self.verbose = True
        elif token[1] == 'x':
          # the file_ext switch
          if token_len > 2:
            # whitespace optional
            self.file_ext = token[2:]
          elif n < argc:
            # whitespace allowed
            n += 1
            self.file_ext = argv[n]
        elif token[1] == '?':
          # the help switch
          usage = True
        else:
          # unknown switch
          usage = True
      n += 1

    if n < argc:
      # parse word_file
      self.word_file = argv[n]
      # word_file is optional
      n += 1

    if n < argc:
      # parse art_path
      self.art_file = argv[n]
      # art_path is optional
      n += 1
    if n < argc:
      # superfluous argument specified
      usage = True

    if self.verbose:
      self.show()

    if usage:
      print(f'Usage: python {script_name} [-t trials] [-v] [-x file_ext] [word_file [art_file]]')

    return not usage

  def show(self):
    print(f'word_file: {self.word_file}')
    print(f'art_file: {self.art_file}')
    print(f'file_ext: {self.file_ext}')
    print(f'verbose: {self.verbose}')


def arg_msg(s, name='argument'):
  msg = 'Invalid '
  msg += name
  msg += ': '
  msg += s
  return msg
