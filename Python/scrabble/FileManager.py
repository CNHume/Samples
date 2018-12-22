# -*- coding: utf-8 -*-
# Copyright (C) 2018, Christopher Hume.  All rights reserved.
#
# You should have received a copy of the MIT License along with this program.
# If not, see https://opensource.org/licenses/MIT.
#
# 2018-07-11 CNHume Created FileManager class
#
import os
from datetime import datetime
import time

from Command import Command

class FileManager(object):
  """FileManager Class"""

  def __init__(self, file_path, file_ext, verbose=False):
    self.records = []
    self.file_path = file_path
    self.file_ext = file_ext
    self.verbose = verbose
    self.filename = FileManager.expanded_filename(file_path, file_ext)

  def load(self):
    """Load records from the file indicated by file_path and file_ext"""
    # Mark load start time
    load_dt0 = datetime.now()
    if self.verbose:
      print(u'{0} starting load'.format(str(load_dt0)[:-3]))
      print(u'{0}: {1}'.format(u'filename', self.filename))

    # Mark elapsed start time
    elapsed_t0 = time.time()

    if FileManager.isfile(self.filename):
      with open(self.filename, 'r') as text_file:
        # Deserialize records from the file, removing newlines
        newlines = text_file.readlines()
        self.records = map(lambda line: line.splitlines()[0], newlines)

    self.length = len(self.records)

    elapsed_t1 = time.time()
    elapsed_delta = elapsed_t1 - elapsed_t0
    load_dt1 = datetime.now()

    if self.verbose:
      # Report counts and times
      print(u'{0} finished load'.format(str(load_dt1)[:-3]))
      print(u'{0:.3f} sec elapsed'.format(round(elapsed_delta, 3)))
      if elapsed_delta > 0:
        rate = self.length / elapsed_delta
        scale = 1e3
        print(u'Loaded {0} records at {1:.3f} KHz'.format(self.length, round(rate / scale, 3)))
      else:
        print(u'Loaded {0} records'.format(self.length))
        
  def paragraphs(self):
    return FileManager.splitter(self.records)

  @staticmethod  
  def splitter(records):
    result = []
    sublist = []
    for record in records:
      if record:
        sublist.append(record)
      elif sublist:                     # Paragraphs must have one non-blank line
        result.append(sublist)
        sublist = []
    if sublist:
      result.append(sublist)
    return result

  @staticmethod
  def expanded_filename(file_path, file_ext):
    expanded_path = os.path.expanduser(file_path)
    (root, ext) = os.path.splitext(expanded_path)
    filename = expanded_path if ext else u'{0}.{1}'\
      .format(expanded_path, file_ext)
    return filename
  
  @staticmethod
  def isfile(filename):
    return os.path.isfile(filename)
