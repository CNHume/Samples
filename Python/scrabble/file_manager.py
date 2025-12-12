# Copyright (C) 2018, Christopher Hume.  All rights reserved.
#
# You should have received a copy of the MIT License along with this program.
# If not, see https://opensource.org/licenses/MIT.
#
# 2018-07-11 CNHume Created FileManager class
#
import os
import errno
from datetime import datetime
import time

DOT = '.'
NEWLINE = '\n'


class FileManager(object):
  """FileManager Class"""

  def __init__(self, file_path, file_ext, hasHeader=False, verbose=False):
    self.file_path = file_path
    self.file_ext = file_ext
    self.hasHeader = hasHeader
    self.verbose = verbose
    self.header = None
    self.records = None
    self.length = 0

  def expand_filename(self, filename):
    expanded_path = os.path.expanduser(self.file_path)
    full_path = os.path.join(expanded_path, filename)
    root, ext = os.path.splitext(full_path)
    filename = full_path if ext else DOT.join([full_path, self.file_ext])
    return filename

  def load(self, file_name):
    """Load records from the file indicated by file_path and file_ext"""
    filename = self.expand_filename(file_name)
    # Mark load start time
    load_dt0 = datetime.now()
    if self.verbose:
      print(f'{str(load_dt0)[:-3]} starting load')
      print(f'{filename=}')

    # Mark elapsed start time
    elapsed_t0 = time.time()

    #
    # Allow FileNotFoundError to be raised
    #
    with open(filename, 'r', encoding='utf-8') as input_file:
      # Deserialize records from the file, removing newlines
      newlines = input_file.readlines()
      records = [line.splitlines()[0] for line in newlines]
      if self.hasHeader:
        self.header = records[0]
        self.records = records[1:]
      else:
        self.header = None
        self.records = records

    self.length = len(self.records) if self.records else 0

    elapsed_t1 = time.time()
    elapsed_time = elapsed_t1 - elapsed_t0
    load_dt1 = datetime.now()

    if self.verbose:
      # Report counts and times
      print(f'{str(load_dt1)[:-3]} finished load')
      print(f'{elapsed_time=:.3f} sec')
      if elapsed_time > 0:
        rate = self.length / elapsed_time
        scale = 1e3
        print(f'Loaded {self.length} records at {rate / scale:.3f} KHz')
      else:
        print(f'Loaded {self.length} records')

  def save(self, file_name, records):
    """Save records into the file indicated by file_path and file_ext"""
    self.records = records
    self.length = len(self.records) if self.records else 0

    filename = self.expand_filename(file_name)
    # Mark save start time
    save_dt0 = datetime.now()

    if self.verbose:
      print(f'{str(save_dt0)[:-3]} starting save')
      print(f'{filename=}')

    # Mark elapsed start time
    elapsed_t0 = time.time()

    if records:
      ensureDirectory(filename)

      with open(filename, 'w', encoding='utf-8') as output_file:
        # Serialize records to the file
        line = NEWLINE.join(records)
        output_file.write(line)
        output_file.write(NEWLINE)

    elapsed_t1 = time.time()
    elapsed_time = elapsed_t1 - elapsed_t0
    save_dt1 = datetime.now()

    if self.verbose:
      # Report counts and times
      print(f'{str(save_dt1)[:-3]} finished save')
      print(f'{elapsed_time=:.3f} sec')
      if elapsed_time > 0:
        rate = self.length / elapsed_time
        scale = 1e3
        print(f'Saved {self.length} records at {rate / scale:.3f} KHz')
      else:
        print(f'Saved {self.length} records')

  def paragraphs(self):
    return splitter(self.records)


def splitter(records):
  result = []
  sublist = []
  for record in records:
    if record:
      sublist.append(record)
    elif sublist:
      # Paragraphs must have one non-blank line
      result.append(sublist)
      sublist = []
  if sublist:
    result.append(sublist)
  return result


def isfile(filename):
  return os.path.isfile(filename)


def ensureDirectory(filename):
  if not os.path.exists(os.path.dirname(filename)):
    try:
      os.makedirs(os.path.dirname(filename))
    except OSError as ex:
      # Guard against race condition
      if ex.errno != errno.EEXIST:
        raise


def filestem(filename):
  basename = os.path.basename(filename)
  root, ext = os.path.splitext(basename)
  return root
