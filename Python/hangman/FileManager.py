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
import string

from Command import Command

class FileManager(object):
  """FileManager Class"""
  DOT = u'.'

  def __init__(self, file_path, file_ext, verbose=False):
    self.records = []
    self.file_path = file_path
    self.file_ext = file_ext
    self.verbose = verbose

  def expand_filename(self, filename):
    expanded_path = os.path.expanduser(self.file_path)
    full_path = os.path.join(expanded_path, filename)
    root, ext = os.path.splitext(full_path)
    filename = full_path if ext else FileManager.DOT.join([full_path, self.file_ext])
    return filename

  def load(self, file_name):
    """Load records from the file indicated by file_path and file_ext"""
    filename = self.expand_filename(file_name)
    # Mark load start time
    load_dt0 = datetime.now()
    if self.verbose:
      print(u'{0} starting load'.format(str(load_dt0)[:-3]))
      print(u'{0}: {1}'.format(u'filename', filename))

    # Mark elapsed start time
    elapsed_t0 = time.time()

    if FileManager.isfile(filename):
      with open(filename, 'r') as input_file:
        # Deserialize records from the file, removing newlines
        newlines = input_file.readlines()
        self.records = map(lambda line: line.splitlines()[0], newlines)

    self.length = len(self.records)

    elapsed_t1 = time.time()
    elapsed_time = elapsed_t1 - elapsed_t0
    load_dt1 = datetime.now()

    if self.verbose:
      # Report counts and times
      print(u'{0} finished load'.format(str(load_dt1)[:-3]))
      print(u'{0:.3f} sec elapsed'.format(round(elapsed_time, 3)))
      if elapsed_time > 0:
        rate = self.length / elapsed_time
        scale = 1e3
        print(u'Loaded {0} records at {1:.3f} KHz'.format(self.length, round(rate / scale, 3)))
      else:
        print(u'Loaded {0} records'.format(self.length))
        
  def save(self, records, file_name):
    """Save records into the file indicated by file_path and file_ext"""
    self.records = records
    self.length = len(self.records)
    filename = self.expand_filename(file_name)
    # Mark save start time
    save_dt0 = datetime.now()

    if self.verbose:
      print(u'{0} starting save'.format(str(save_dt0)[:-3]))
      print(u'{0}: {1}'.format(u'filename', filename))

    # Mark elapsed start time
    elapsed_t0 = time.time()

    if FileManager.isfile(filename):
      with open(filename, 'w') as output_file:
        # Serialize records to the file
        output_file.writelines()

    elapsed_t1 = time.time()
    elapsed_time = elapsed_t1 - elapsed_t0
    save_dt1 = datetime.now()

    if self.verbose:
      # Report counts and times
      print(u'{0} finished save'.format(str(save_dt1)[:-3]))
      print(u'{0:.3f} sec elapsed'.format(round(elapsed_time, 3)))
      if elapsed_time > 0:
        rate = self.length / elapsed_time
        scale = 1e3
        print(u'Saved {0} records at {1:.3f} KHz'.format(self.length, round(rate / scale, 3)))
      else:
        print(u'Saved {0} records'.format(self.length))

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
  def isfile(filename):
    return os.path.isfile(filename)

  @staticmethod
  def filestem(filename):
    basename = os.path.basename(filename) 
    root, ext = os.path.splitext(basename)
    return root
