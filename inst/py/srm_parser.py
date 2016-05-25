# -*- coding: utf-8 -*-
"""
A script for parsing SRM power control files.

Developed with python 2.7.11

Based on:
https://github.com/GoldenCheetah/GoldenCheetah/blob/master/src/FileIO/SrmRideFile.cpp
"""

# %% ------------------------------------------------------ %%

from __future__ import division
import sys
from struct import unpack
import csv

# https://docs.python.org/3/library/struct.html


def read_byte(srmfile):
    return unpack("<B", srmfile.read(1))[0]


def read_short(srmfile):  # "h" for s_h_ort.
    return unpack("<H", srmfile.read(2))[0]


def read_signed_short(srmfile):  # Lower case for signed.
    return unpack("<h", srmfile.read(2))[0]


def read_long(srmfile):
    return unpack("<L", srmfile.read(4))[0]


def read_signed_long(srmfile):
    return unpack("<l", srmfile.read(4))[0]


def old_byte_unpack(srmfile):
    spd_pwr = unpack("<BBB", srmfile.read(3))
    kph = ((spd_pwr[1] & 0xf0) << 3 | (spd_pwr[0] & 0x7f)) * 3/26
    watts = (spd_pwr[1] & 0x0f) | (spd_pwr[2] << 0x4)
    return (watts, kph)


def strip_nulls(bin):
    return bin.partition(b'\0')[0]


def srm_header(srmfile):
    header = {
      "days_since_1880": read_short(srmfile),
      "wheel_circum": read_short(srmfile),
      # Recording interval (1 / Hz) in seconds.
      "recording_interval": read_byte(srmfile) / read_byte(srmfile),
      "block_count": read_short(srmfile),
      "marker_count": read_short(srmfile),
      "padding": read_byte(srmfile),
      "comment_len": read_byte(srmfile)
    }
    header.pop("padding")
    return header


def srm_markers(srmfile, marker_count, version):
    markers = []
    marker_comment_len = 3 if version < 6 else 255

    # SRM files **always contain at least one marker**
    # that encompasses the entire srmfile.
    for __ in range(0, marker_count + 1):
        this_marker = {
          # Athlete's name.
          "note": strip_nulls(srmfile.read(marker_comment_len)),

          "active": read_byte(srmfile),   # Bool.
          "start": (read_short if version < 9 else read_long)(srmfile),
          "end": (read_short if version < 9 else read_long)(srmfile),
          "average_watts": read_short(srmfile),
          "average_hr": read_short(srmfile),
          "average_cadence": read_short(srmfile),
          "average_speed": read_short(srmfile),
          "pwc150": read_short(srmfile)
        }
        # Some data checking:
        limits = [1 if val < 1 else val   # Neither < 1.
                  for key, val in this_marker.items()
                  if key in ["start", "end"]]
        # Some srmwin versions wrote markers with start > end.
        this_marker.update({"start": min(limits), "end": max(limits)})

        markers.append(this_marker)

    return markers


def srm_blocks(srmfile, block_count, version):
    block_data_count = 0
    blocks = []
    for __ in range(block_count):
        # First block is the start time.
        this_block = {
          "sec_since_midnight": read_long(srmfile) / 100,  # hsec to sec.
          "data_chunks": (read_short if version < 9 else read_long)(srmfile)
        }
        blocks.append(this_block)
        block_data_count += this_block["data_chunks"]

    blocks.append(block_data_count)  # To be retrieved later.
    return blocks


def srm_data_chunks(srmfile, nrows, version, header, markers, blocks):

    fields = ["watts", "cad", "hr", "kph", "alt", "temp", "km",
              "lat", "lon", "timeoffset", "lap"]

    # Return a JSON-style table.
    out = [{} for __ in range(nrows)]

    # Cleaner...
    rec_interval_sec = header["recording_interval"]

    # Need to do some initialising...
    i_marker = 1 if header["marker_count"] else 0   # Any laps?
    current_marker = markers[i_marker]

    i_block, new_block, block_progress = 0, 0, 0
    current_block = blocks[i_block]

    lap = 1

    for i in range(nrows):
        if version < 7:
            watts, kph = old_byte_unpack(srmfile)
            cad, hr = unpack("<BB", srmfile.read(2))
            alt, temp = ["NA"]*2  # Missing.
        else:
            watts, cad, hr, kph, alt, temp = unpack("<HBBllh",
                                                    srmfile.read(14))
            temp *= 0.1
            kph = 0 if kph < 0 else kph * 3.6 / 1000

        # Deal with positional coordinates.
        if version is 9:
            latlon = unpack("<ll", srmfile.read(8))
            lat, lon = [x * 180.0 / 0x7fffffff for x in latlon]
        else:
            lat, lon = ["NA"]*2

        # And generate a distance field.
        km = rec_interval_sec * kph / 3600

        # ---- Time offset -------------------------------------------------
        # Since the Powercontrol unit may go into power save mode when idle,
        # the data may represent multiple discontinuous blocks. Each data
        # block header contains the time stamp.

        # Have we moved on to the next block?
        if block_progress >= current_block["data_chunks"]:
            i_block += 1
            current_block = blocks[i_block]
            block_progress = 0  # Reset.
            new_block = 1       # Flag.

        if i and new_block:
            timeoffset = current_block["sec_since_midnight"]
            new_block = 0  # Reset.

        elif i:  # Increment previous value by recording interval.
            timeoffset = out[i-1]["timeoffset"] + rec_interval_sec

        else:  # i == 0 (use start time).
            timeoffset = blocks[0]["sec_since_midnight"]

        block_progress += 1  # Bump.

        # ---- Laps --------------------------------------------------------
        i_marker_fine = i_marker < (len(markers) - 1)  # In range?

        if i_marker_fine and (i == current_marker["end"]):
            i_marker += 1
            current_marker = markers[i_marker]
            lap += 1
        # ! Markers count from 1.
        if i_marker_fine and not i and (i == current_marker["start"] - 1):
            lap += 1

        # ---- Finally -----------------------------------------------------
        these_cells = map(locals().get, fields)
        out[i] = {key: value for key, value in zip(fields, these_cells)}

        if i:
            out[i]["km"] += out[i-1]["km"]  # Cumulative distance covered.

    return out


# %% ------------------------------------------------------ %%


def main(file_path):

    with open(file_path, "rb") as srmfile:

        magic = srmfile.read(4)
        if magic[:3] != "SRM":
            raise IOError("SRM file not recognised.")

        version = int(magic[3])

        header = srm_header(srmfile)

        srmfile.seek(70, 1)  # Skim over the 70 byte comment.

        markers = srm_markers(srmfile, header["marker_count"], version)
        blocks = srm_blocks(srmfile, header["block_count"], version)
        block_data_count = blocks.pop()

        srmfile.seek(4, 1)  # Skip over calibration data.

        data_count = read_short(srmfile) if version < 9 else read_long(srmfile)

        srmfile.seek(1, 1)  # Padding.

        nrows = max([data_count, block_data_count])

        chunks = srm_data_chunks(srmfile, nrows, version,
                                 header, markers, blocks)

    return chunks

# %% ------------------------------------------------------ %%
if __name__ == "__main__":

    chunks = main(sys.argv[1])

    fields = ["watts", "cad", "hr", "kph",
              "alt", "temp", "lat", "lon",
              "timeoffset", "km", "lap"]

    # Sink chunks to a .csv file
    with open(sys.argv[2], 'wb') as csvfile:
        csv_writer = csv.writer(csvfile, delimiter='\t', quotechar='"',
                                quoting=csv.QUOTE_MINIMAL)

        csv_writer.writerow(fields)  # Header.

        for row in chunks:
            this_row = map(row.get, fields)
            csv_writer.writerow(this_row)
