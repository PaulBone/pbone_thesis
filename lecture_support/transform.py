#!/usr/bin/python -d
# vim: ts=4 sw=4 et ft=python

"""
Transform a .orig file into into a .tex file.

Note to maintainers:
This program gets a rating above 9.5 out of 10 from pylint;
please keep it that way.

Most of the complaints still raised by pylint are related to the fact that
the code uses a structured programming style, not an object-oriented style.
I chose the structured programming style because e.g. outputting a slide
or a section heading updates several local state variables, and any methods
for doing such printing would have to be passed the current values of these
state variables, and would also have to return their possibly updated values.
All this parameter passing would obscure the flow of data and infuence
between the various parts of the relevant loops (in e.g. print_notes)
by placing it behind an abstraction barrier.
"""

import copy
import getopt
import os
import sys

class Talk:
    """
    This structure contains all the information we have about the talk
    from configuration files.
    """

    def __init__(self, basename):
        """
        Read the talk configuration files, and return a structure
        giving their contents.
        """

        self.basename = basename
        self.title = \
            self.read_conf_file("title", "UNKNOWN TITLE")
        self.date = \
            self.read_conf_file("date", "UNKNOWN DATE")
        self.institute = \
            self.read_conf_file("institute", "UNKNOWN INSTITUTE")
        self.author = \
            self.read_conf_file("author", "UNKNOWN AUTHOR")
        self.author_long = \
            self.read_conf_file("author_long", "UNKNOWN AUTHOR")
        self.organization = \
            self.read_conf_file("organization", "The University of Melbourne")
        self.logos = \
            self.read_whole_file("logos", "")

    def __str__(self):
        """ Print the talk parameters. """

        desc = ""
        desc = desc + "Title:        %s\n" % self.title
        desc = desc + "Institute:    %s\n" % self.institute
        desc = desc + "Date:         %s\n" % self.date
        desc = desc + "Author:       %s\n" % self.author
        desc = desc + "Author long:  %s\n" % self.author_long
        desc = desc + "Organization: %s\n" % self.organization
        return desc

    def read_conf_file(self, name, default):
        return read_conf_file(os.path.join(self.basename, name), default)

    def read_whole_file(self, name, default):
        return read_whole_file(os.path.join(self.basename, name), default)

class Beamer:
    """
    This structure contains all the beamer parameters we have
    from configuration files.
    """

    def __init__(self, basename):
        """
        Read the beamer configuration files, and return a structure
        giving their contents.
        """

        self.basename = basename
        self.beamer_options = \
            self.read_conf_file("beamer_options", "")
        self.beamer_theme = \
            self.read_conf_file("beamer_theme", "CambridgeUS")
        self.beamer_inner_theme = \
            self.read_conf_file("beamer_inner_theme", "default")
        self.beamer_outer_theme = \
            self.read_conf_file("beamer_outer_theme", "default")
        self.beamer_color_theme = \
            self.read_conf_file("beamer_color_theme", "dolphin")
        self.beamer_font_theme = \
            self.read_conf_file("beamer_font_theme", "structurebold")

    def __str__(self):
        """ Print the beamer parameters. """

        desc = ""
        desc = desc + "Options:     %s\n" % self.beamer_options
        desc = desc + "Theme:       %s\n" % self.beamer_theme
        desc = desc + "Inner theme: %s\n" % self.beamer_inner_theme
        desc = desc + "Outer theme: %s\n" % self.beamer_outer_theme
        desc = desc + "Color theme: %s\n" % self.beamer_color_theme
        desc = desc + "Font theme:  %s\n" % self.beamer_font_theme
        return desc
    
    def read_conf_file(self, name, default):
        return read_conf_file(os.path.join(self.basename, name), default)

class NumLine:
    """
    We attach a filename and a linenumber to each line we read in.
    This way, we can generate accurate error messages even though
    (a) we filter out some lines during the processing of SLIDESONLY
    and NOTESONLY directives, and (b) some lines we get from other files
    we read when handling INCLUDE directives.
    """

    def __init__(self, filename, line_number, line):
        """ Create a numbered line. """

        self.file = filename
        self.number = line_number
        self.line = line

    def __str__(self):
        """ Print the details of a numbered line. """

        return "%s, %s: %s" % (self.file, self.number, self.line)

class Section:
    """ A section title. """

    def __init__(self, section_number, section_name):
        """ Create a section title. """

        self.sect_section_number = section_number
        self.sect_section_name = section_name

    def __str__(self):
        """ Print the details of a section title. """

        return "SECTION %d: %s\n" \
            % (self.sect_section_number, self.sect_section_name)

class Subsection:
    """ A subsection title. """

    def __init__(self, section_number, subsection_number, subsection_name):
        """ Create a section title. """

        self.subsect_section_number = section_number
        self.subsect_subsection_number = subsection_number
        self.subsect_subsection_name = subsection_name

    def __str__(self):
        """ Print the details of a subsection title. """

        return "SUBSECTION %d.%d %s\n" \
            % (self.subsect_section_number, self.subsect_subsection_number, \
            self.subsect_subsection_name)

class Slide:
    """
    An ordinary, quiz or demo slide, shown by the slide_kind field being
    SLIDE, QUIZ and DEMO respectively.
    """

    def __init__(self, kind, section_number, subsection_number, title, \
            options, columns, lines, notes):
        """ Create a slide. """

        self.slide_kind = kind
        self.slide_section_number = section_number
        self.slide_subsection_number = subsection_number
        self.slide_title = title
        self.slide_options = options
        self.slide_columns = columns
        self.slide_lines = lines
        self.slide_notes = notes

    def __str__(self):
        """ Print the details of a slide. """

        option_str = self.slide_options
        if self.slide_columns != 2:
            if option_str == "":
                option_str = "ONECOLUMN"
            else:
                option_str = option_str + " ONECOLUMN"
        if option_str != "":
            option_str = " (%s)" % option_str

        head = "%s in %d.%d: %s%s\n" % (self.slide_kind, \
            self.slide_section_number, self.slide_subsection_number, \
            self.slide_title, option_str)
        lines = "LINES:\n" + "\n".join(self.slide_lines) + "\n"
        notes = "NOTES:\n" + "\n".join(self.slide_notes) + "\n"
        if self.slide_notes == []:
            desc = head + lines
        else:
            desc = head + lines + notes

        return desc

    def slide_prefix(self):
        """
        Return the prefix (if any) we should put on the slide's title.
        """

        if self.slide_kind == "SLIDE":
            prefix = ""
        elif self.slide_kind == "QUIZ":
            prefix = "QUIZ: "
        elif self.slide_kind == "DEMO":
            prefix = "DEMO: "
        else:
            prefix = "UNKNOWN SLIDE TYPE: "

        return prefix

class Item:
    """
    We parse the input .orig file into a list of items. There are three types
    of items: section titles, subsection titles, and slides, shown by the kind
    field being SECTION, SUBSECTION and SLIDE respectively.
    The types of the data fields is Section, Subsection and Slide respectively.
    """

    def __init__(self, kind, data):
        """ Create an item. """

        self.kind = kind
        self.data = data

    def __str__(self):
        """ Print the details of an item. """

        return str(self.data)

###############################################################################

def usage(msg):
    """ Print the usage message. """

    print >> sys.stderr, msg
    print >> sys.stderr, \
        "usage: transform [-adnq] [-tN] beamer basename"

def main():
    """
    Transform the .orig file referred to by the basename argument
    into a .tex file.
    """

    # DEBUG print >> sys.stderr, sys.argv

    status = 0

    try:
        (opts, args) = getopt.gnu_getopt(sys.argv[1:], "abindqr:t:")
    except getopt.GetoptError:
        usage("unknown option")
        sys.exit(2)

    bold_slide_titles = False
    include_notes = True
    ignored_kinds = []
    instructor_version = False
    tab_replacement = "    "        # four spaces
    for (opt, arg) in opts:
        if opt == "-a":
            x = 0
        elif opt == "-b":
            bold_slide_titles = True
            # DEBUG print "setting bold_slide_titles"
        elif opt == "-i":
            instructor_version = True
            # DEBUG print "setting instructor_version"
        elif opt == "-n":
            include_notes = False
            # DEBUG print "setting include_notes"
        elif opt == "-d":
            ignored_kinds = ignored_kinds + ["DEMO"]
            # DEBUG print "ignoring demos"
        elif opt == "-q":
            ignored_kinds = ignored_kinds + ["QUIZ"]
            # DEBUG print "ignoring quizes"
        elif opt == "-t":
            tab_replacement = " " * int(arg)
            # DEBUG print "% using %d-space tabs" % arg
        else:
            usage("unknown option")
            sys.exit(2)

    # DEBUG print >> sys.stderr, args

    if len(args) == 2:
        [target, basename] = args

        if target == "beamer":
            target_select = ["SLIDESONLY", "BEAMERONLY"]
            pic_prefix = "pb_"
        else:
            usage("unknown target")
            sys.exit(2)

        if instructor_version:
            instructor_select = ["INSTRUCTORONLY"]
        else:
            instructor_select = []

        select = target_select + instructor_select

        origname = basename + ".orig"
        (items, msgs, errors) = \
            parse_slide_file(origname, tab_replacement, select)
        if errors > 0:
            for msg in msgs:
                print >> sys.stderr, msg
            status = 1
        else:
            talk = Talk(basename)
            items = filter_items(target, ignored_kinds, include_notes, \
                pic_prefix, items)
            if target == "beamer":
                beamer = Beamer(basename)
                status = print_beamer(talk, beamer, items)
            else:
                usage("unknown target")
                sys.exit(2)

    else:
        usage("incorrect number of arguments")
        status = 1

    sys.exit(status)

###############################################################################

def try_open_file(filename):
    """
    Try to open the named file, and return a (fp, success) pair. If successful,
    success will be True and fp will be valid. If unsuccessful, success will be
    False and fp will NOT be valid.
    """

    try:
        try_fp = open(filename)
        success = True
    except IOError:
        try_fp = sys.stdin
        success = False

    return (try_fp, success)

def print_use_packages(target):
    """
    Print the \usepackage directives needed by the .tex file we are generating.
    Include lecture_common and lecture_<target> if they exist, and include
    the contents of the packages list file from the talk directory.
    """

    package_name = "lecture_common"
    (pack_fp, success) = try_open_file("styles/" + package_name + ".sty")
    if success:
        print "\\usepackage{%s}" % package_name
        pack_fp.close()

    package_name = "lecture_" + target
    (pack_fp, success) = try_open_file("styles/" + package_name + ".sty")
    if success:
        print "\\usepackage{%s}" % package_name
        pack_fp.close()

    (pack_fp, success) = try_open_file("talk/packages")
    if success:
        package_lines = pack_fp.readlines()
        for package_line in package_lines:
            package_line = package_line[:-1]
            if len(package_line) > 0 and package_line[0] != "%":
                print package_line
        pack_fp.close()

def read_conf_file(filename, default):
    """
    Read the named configuration file if it exists. Filter out any lines that
    start with #, and return the remaining contents as a string. If the file
    does not exist, return the supplied default contents.
    """

    (conf_fp, success) = try_open_file(filename)
    if success:
        lines = conf_fp.readlines()
        conf_fp.close()
        for line in lines:
            if len(line) > 1 and line[0] != "#":
                return line[:-1]
    return default

def read_whole_file(filename, default):
    """
    Read the named file if it exists. return its contents as a string.
    If the file does not exist, return the supplied default contents.
    """
    (fp, success) = try_open_file(filename)
    if success:
        contents = fp.read()
        fp.close()
        return contents
    return default

###############################################################################

def get_numbered_lines(filename, select, tab_replacement):
    """
    Read in the named file. Process INCLUDE lines. Filter out lines controlled
    by BEAMERONLY, SEMINARONLY, SLIDESONLY, NOTESONLY and/or INSTRUCTORONLY
    if the starting control word is not in select.

    Return the tuple (numbered_lines, msgs, errors). Numbered_lines will be
    a representation of the data read from the file. If there are any malformed
    INCLUDE Lines or mismatched ONLY/ENDONLY lines, msgs will include an error
    messages for each one, and error will tell you have many such errors
    there have been. Otherwise, msgs will be the empty list and errors
    will be 0.
    """

    file_fp = open(filename)
    raw_lines = file_fp.readlines()
    file_fp.close()

    line_number = 0
    numbered_lines = []

    for raw_line in raw_lines:
        raw_line = raw_line[:-1]
        line_number = line_number + 1
        numbered_line = NumLine(filename, line_number, raw_line)
        numbered_lines = numbered_lines + [numbered_line]

    incl_msgs = []
    incl_errors = 0
    incl_numbered_lines = []
    for numbered_line in numbered_lines:
        words = numbered_line.line.split()
        len_words = len(words)
        if len_words > 0 and words[0] == "INCLUDE":
            if len_words > 1:
                incl_filename = words[len_words - 1]
                words = words[1:-1]
                # We have already handled the first word (INCLUDE)
                # and the last word (the filename).
                len_words = len_words - 2

                # Process all the words between the INCLUDE and the filename,
                # which may ask for extra processing of the included lines.
                unindent = 0;
                cur_word = 0
                while cur_word < len_words:
                    if words[cur_word] == "UNINDENT":
                        if cur_word + 1 < len_words:
                            unindent = int(words[cur_word + 1])
                            cur_word = cur_word + 2
                        else:
                            incl_errors = incl_errors + 1
                            incl_msg = "%s, %d: UNINDENT without number" % \
                                (numbered_line.file, numbered_line.number)
                            incl_msgs = incl_msgs + [incl_msg]
                    else:
                        incl_errors = incl_errors + 1
                        incl_msg = \
                            "%s, %d: %s is not an option for INCLUDEs" % \
                            (numbered_line.file, numbered_line.number, \
                            words[cur_word])
                        incl_msgs = incl_msgs + [incl_msg]

                # We go on even if we found and reported some problems.

                main_sed_cmd_1 = "'s/\t/%s/g'" % tab_replacement
                main_sed_cmd_2 = "'/HIDE_START/,/HIDE_END/d'"
                main_sed_cmd = "sed -e %s -e %s %s" % \
                    (main_sed_cmd_1, main_sed_cmd_2, incl_filename)
                if unindent == 0:
                    incl_cmd = main_sed_cmd
                else:
                    unindent_str = "." * unindent
                    unindent_cmd = "sed -e 's/^%s//'" % unindent_str
                    incl_cmd = "%s | %s" % (main_sed_cmd, unindent_cmd)

                incl_fp = os.popen(incl_cmd)
                incl_lines = incl_fp.readlines()
                incl_fp.close()
                incl_line_number = 0
                for incl_line in incl_lines:
                    incl_line = incl_line[:-1]
                    incl_line_number = incl_line_number + 1
                    incl_numbered_line = \
                        NumLine(incl_filename, incl_line_number, incl_line)
                    incl_numbered_lines = \
                        incl_numbered_lines + [incl_numbered_line]
            else:
                incl_errors = incl_errors + 1
                incl_msg = "%s, %d: INCLUDE without filename" % \
                    (numbered_line.file, numbered_line.number)
                incl_msgs = incl_msgs + [incl_msg]

            cur_word = 2;
            unindent = 0;
            while cur_word < len_words:
                if words[cur_word] == "UNINDENT":
                    if cur_word + 1 < len_words:
                        unindent = words[cur_word + 1]
                        cur_word = cur_word + 2
                    else:
                        incl_errors = incl_errors + 1
                        incl_msg = "%s, %d: bad INCLUDE" % \
                            (numbered_line.file, numbered_line.number)
                        incl_msgs = incl_msgs + [incl_msg]
        else:
            incl_numbered_lines = incl_numbered_lines + [numbered_line]

    (final_numbered_lines, select_msgs, select_errors) = \
        filter_nested_selects(incl_numbered_lines, select)

    all_msgs = incl_msgs + select_msgs
    all_errors = incl_errors + select_errors
    return (final_numbered_lines, all_msgs, all_errors)

def filter_nested_selects(numbered_lines, select):
    """
    Filter out lines controlled by BEAMERONLY, SEMINARONLY, SLIDESONLY,
    NOTESONLY and/or INSTRUCTORONLY if the starting control word is not
    in select.

    Return the tuple (numbered_lines, msgs, errors). Numbered_lines will be
    a representation of the data read from the file. If there are any malformed
    INCLUDE Lines or mismatched ONLY/ENDONLY lines, msgs will include an error
    messages for each one, and error will tell you have many such errors
    there have been. Otherwise, msgs will be the empty list and errors
    will be 0.
    """

    begin_onlys = \
        ["BEAMERONLY", "SEMINARONLY", "SLIDESONLY", "NOTESONLY",
        "INSTRUCTORONLY"]

    msgs = []
    errors = 0

    final_lines = []
    num_numbered_lines = len(numbered_lines)
    line_index = 0
    while line_index < num_numbered_lines:
        numbered_line = numbered_lines[line_index]
        line = numbered_line.line
        if not line in begin_onlys:
            final_lines = final_lines + [numbered_line]
            line_index = line_index + 1
        else:
            scope_lines = []
            begin_line = line
            begin_file_name = numbered_line.file
            begin_line_number = numbered_line.number
            end_line = "END" + line
            line_index = line_index + 1
            in_scope = True
            while line_index < num_numbered_lines and in_scope:
                numbered_line = numbered_lines[line_index]
                line = numbered_line.line
                if line != end_line:
                    scope_lines = scope_lines + [numbered_line]
                else:
                    in_scope = False
                line_index = line_index + 1

            if in_scope:
                errors = errors + 1
                msg = "%s, %d: %s without %s" % \
                    (begin_file_name, begin_line_number, begin_line, end_line)
                msgs = msgs + [msg]

            (filtered_scope_lines, scope_msgs, scope_errors) = \
                filter_nested_selects(scope_lines, select)
            msgs = msgs + scope_msgs
            errors = errors + scope_errors
            if begin_line in select:
                final_lines = final_lines + filtered_scope_lines

    return (final_lines, msgs, errors)

def parse_slide_file(filename, tab_replacement, select):
    """
    Read in the named file. Filter out slides-only lines if select != SLIDES,
    and filter out notes-only lines if select != NOTES.

    Return the tuple (items, msgs, errors). Items is a list of items, where
    each item is a section title, a subsection title, or a slide of any of
    three types (ordinary slide, quiz or demo). Msgs will be a list of error
    and/or warning messages. Error will be a count of the number of errors.
    (It is possible for error to be 0 even if msgs is not the empty list,
    if msgs contains warnings.)

    There are several kinds of errors that this code looks for. They are
    all about malformed input.
    """

    next_section_number = 1

    begin_slide_words = ["SLIDE", "QUIZ", "DEMO"]
    end_slide_words = ["ENDSLIDE", "ENDQUIZ", "ENDDEMO"]
    next_slide_words = begin_slide_words + end_slide_words + \
        ["SECTION", "SUBSECTION"]

    items = []

    (numbered_lines, msgs, errors) = \
        get_numbered_lines(filename, select, tab_replacement)

    cur_section_number = -1
    cur_section_name = ""
    cur_subsection_number = 0
    cur_subsection_name = ""

    num_lines = len(numbered_lines)
    cur_line_number = 0

    cur_line_number = \
        skip_blank_lines(numbered_lines, num_lines, cur_line_number)
    while cur_line_number < num_lines:
        have_error = False
        line = numbered_lines[cur_line_number].line
        words = line.split()
        if len(words) == 0:
            # This shouldn't happen, due to skip_blank_lines.
            cmd = ""
        else:
            cmd = words[0]

        # print "cur %d, num_lines %d" % (cur_line_number, num_lines)
        # print words
        if cmd == "SECTION":
            if len(words) < 3:
                have_error = True
                msg = "%s, %d: bad SECTION" % \
                    (filename, numbered_lines[cur_line_number].number)
                msgs = msgs + [msg]

            if not have_error:
                new_section_number = int(words[1])
                new_section_number_ok = False
                if cur_section_number == -1:
                    new_section_number_ok = True
                elif new_section_number == cur_section_number + 1:
                    new_section_number_ok = True

                if not new_section_number_ok:
                    have_error = True
                    msg = \
                        "%s, %d: SECTION %d followed by SECTION %d" % \
                        (filename, numbered_lines[cur_line_number].number,
                        cur_section_number, new_section_number)
                    msgs = msgs + [msg]

                cur_section_number = new_section_number
                cur_section_name = " ".join(words[2:])
                cur_subsection_number = int(0)
                cur_subsection_name = ""

                sect = Section(cur_section_number, cur_section_name)
                item = Item("SECTION", sect)

        elif cmd == "SUBSECTION":
            if len(words) < 2:
                have_error = True
                msg = "%s, %d: bad SUBSECTION" % (filename, cur_line_number)
                msgs = msgs + [msg]

            if not have_error:
                cur_subsection_number = cur_subsection_number + 1
                cur_subsection_name = " ".join(words[1:])

                subsect = Subsection(cur_section_number, \
                    cur_subsection_number, cur_subsection_name)
                item = Item("SUBSECTION", subsect)

        elif cmd in begin_slide_words:
            have_error = False
            slide_columns = 2
            slide_options = []

            cur_word = 1
            num_words = len(words)

            while cur_word < num_words:
                if words[cur_word] == "ONECOLUMN":
                    slide_columns = 1
                    cur_word = cur_word + 1
                elif words[cur_word] == "OPTION":
                    slide_options = slide_options + [words[cur_word + 1]]
                    cur_word = cur_word + 2
                else:
                    break

            slide_title = " ".join(words[cur_word:])
            if slide_title == "":
                have_error = True
                msg = "%s, %d: bad %s" % (filename, cur_line_number, cmd)
                msgs = msgs + [msg]

            if not have_error:
                end_word = "END" + cmd

                slide_lines = []
                slide_notes = []
                seen_note = False
                while True:
                    next_line_number = cur_line_number + 1
                    if next_line_number >= num_lines:
                        break

                    line = numbered_lines[next_line_number].line
                    line_words = line.split()
                    if len(line_words) == 0:
                        first_word = ""
                    else:
                        first_word = line_words[0]

                    if first_word in next_slide_words:
                        if first_word in end_slide_words:
                            cur_line_number = next_line_number
                            if first_word != end_word:
                                # Don't set have_error; the item is still
                                # valid.
                                msg = "%s, %d: %s ended with %s" % \
                                    (filename, cur_line_number, \
                                    cmd, first_word)
                                msgs = msgs + [msg]
                        break

                    if first_word == "NOTE" and len(line_words) == 1:
                        seen_note = True
                    else:
                        if seen_note:
                            slide_notes = slide_notes + [line]
                        else:
                            slide_lines = slide_lines + [line]

                    cur_line_number = next_line_number

                slide = Slide(cmd, cur_section_number, cur_subsection_number, \
                    slide_title, slide_options, slide_columns, \
                    slide_lines, slide_notes)
                item = Item("SLIDE", slide)

        else:
            # Comment lines and blank lines have already been ignored
            # by skip_blank_lines.
            have_error = True
            msg = "%s, %d: orphan line <%s>" % (filename, cur_line_number, line)
            msgs = msgs + [msg]

        if have_error:
            errors = errors + 1
        else:
            items = items + [item]

        cur_line_number = cur_line_number + 1
        cur_line_number = \
            skip_blank_lines(numbered_lines, num_lines, cur_line_number)

    return (items, msgs, errors)

###############################################################################

def filter_items(target, ignored_kinds, include_notes, pic_prefix, items):
    """
    Remove items that ignored slide types, remove notes from slides
    if they are not needed, and modify the contents of the slides
    if the needed form of the content differs between targets.
    """

    if target != "notes":
        include_notes = False

    filtered_items = []
    for item in items:
        if item.kind == "SECTION":
            filtered_items = filtered_items + [item]
        elif item.kind == "SUBSECTION":
            filtered_items = filtered_items + [item]
        elif item.kind == "SLIDE":
            slide = item.data
            if not slide.slide_kind in ignored_kinds:
                (slide.slide_lines, slides_pause) = \
                    rewrite_and_detect_pause(pic_prefix, slide.slide_lines)
                (slide.slide_notes, notes_pause) = \
                    rewrite_and_detect_pause(pic_prefix, slide.slide_notes)

                if not include_notes:
                    slide.slide_notes = []
                    notes_pause = False

                item.data = slide

                if notes_pause:
                    # Pauses are never useful inside notes, regardless of
                    # the target. We could generate an error message here.
                    slide.slide_notes = filter_out_pause(slide.slide_notes)
                    item.data = slide

                if slides_pause:
                    if target == "beamer":
                        slide.slide_lines = beamerify_pause(slide.slide_lines)
                        item.data = slide
                        filtered_items = filtered_items + [item]
                    elif target == "seminar":
                        expanded_items = expand_out_pause(item)
                        filtered_items = filtered_items + expanded_items
                    elif target == "notes":
                        slide.slide_lines = filter_out_pause(slide.slide_lines)
                        item.data = slide
                        filtered_items = filtered_items + [item]
                else:
                    filtered_items = filtered_items + [item]

    return filtered_items

def beamerify_pause(old_lines):
    """ Replace PAUSE lines with \pause lines. """

    new_lines = []
    for old_line in old_lines:
        if old_line == "PAUSE":
            new_lines = new_lines + ["\pause"]
        else:
            new_lines = new_lines + [old_line]

    return new_lines

def filter_out_pause(old_lines):
    """ Filter out PAUSE lines. """

    new_lines = []
    for old_line in old_lines:
        if old_line != "PAUSE":
            new_lines = new_lines + [old_line]

    return new_lines

def expand_out_pause(item):
    """
    Given a slide item with N PAUSE lines, which has a structure like this:

        line group 1
        PAUSE
        line group 2
        PAUSE
        line group 3

    return a list of N+1 slide items, structured like this:

        slide 1:
        line group 1
        <blank line>

        slide 2:
        line group 1
        <blank line>
        line group 2

        slide 3:
        line group 1
        <blank line>
        line group 2
        <blank line>
        line group 3

    Delete the notes lines from all of these slides except the last.
    """

    slide = item.data
    old_lines = slide.slide_lines

    before_lines = []
    after_lines = []
    before = True
    for old_line in old_lines:
        if old_line == "PAUSE":
            before = False
            after_lines = [""]
        else:
            if before:
                before_lines = before_lines + [old_line]
            else:
                after_lines = after_lines + [old_line]

    if before:
        # There is no PAUSE line.
        return [item]

    first_slide = slide
    later_slide = copy.copy(slide)

    # First, we have a slide with the lines before the PAUSE. We delete any
    # notes on the slide, since we will keep these in the last slide.
    first_slide.slide_lines = before_lines
    first_slide.slide_notes = []
    first_item = Item(item.kind, first_slide)

    # Then, we have a slide with the lines before and after the PAUSE,
    # but not the pause line itself. We keep any notes.
    later_slide.slide_lines = before_lines + after_lines
    later_item = Item(item.kind, later_slide)

    # We process the later slide recursively. If there are any more PAUSE
    # lines, they will be processed, and later_item will be replaced by several
    # items; otherwise, the recursive call will return [later_item].
    return [first_item] + expand_out_pause(later_item)

def rewrite_and_detect_pause(pic_prefix, old_lines):
    """
    Process old_lines. Replace any references to epsfboxes to refer to a
    target-specific version of the postscript file by adding the given prefix
    to their filenames. Also detect whether the lines contain any occurrences
    of PAUSE. The return value is a pair (new_lines, contains_pause).
    """

    contains_pause = False
    new_lines = []
    for old_line in old_lines:
        if old_line[0:9] == "\\epsfbox{":
            new_line = "%s%s%s" % ( old_line[0:9], pic_prefix, old_line[9:] )
        else:
            new_line = old_line

        if new_line == "PAUSE":
            contains_pause = True

        new_lines = new_lines + [new_line]

    return (new_lines, contains_pause)

###############################################################################

def print_beamer(talk, beamer, items):
    """
    Print out the items as a .tex file using the beamer package.
    """

    if beamer.beamer_options == "":
        beamer.beamer_options = ",t"
    else:
        beamer.beamer_options = ",t," + beamer.beamer_options

    print "\\documentclass[t%s]{beamer}" % beamer.beamer_options
    print_use_packages("beamer")
    print "\\mode<presentation>"
    print "\\usetheme{%s}"          % beamer.beamer_theme
    print "\\usecolortheme{%s}"     % beamer.beamer_color_theme
    print "\\usefonttheme{%s}"      % beamer.beamer_font_theme
    print "\\useinnertheme{%s}"     % beamer.beamer_inner_theme
    print "\\useoutertheme{%s}"     % beamer.beamer_outer_theme
    # print "\\usepackage[english]{babel}"
    print ""

    # The author and institute are obviously fakes; they are set up
    # to insert them into the footline.
    author = talk.author
    author_long = talk.author_long
    title = talk.title
    institute = talk.institute
    organization = talk.organization
    logos = talk.logos.split("\n")
    (logos, _) = rewrite_and_detect_pause("pb_", logos)
    logos = "\n".join(logos)
    date = talk.date

    unknown_items = []

    print "\\title{%s}" % title
    print "\\author{%s}" % author
    print "\\institute{%s}" % "UoM \& NICTA"
    # organization.split("\\\\")[0].strip()
    print "\\date{%s}" % date
    print "\\begin{document}"
    print ""

    for item in items:
        if item.kind == "SECTION":
            sect = item.data
            print "\\section{%s}\n" % sect.sect_section_name
            print "\\begin{frame}"
            print "\\frametitle{}\n"
            print "\\begin{center}"
            # print "{\\small %s}\\\\[5mm]" % subj.institute
            # print "{\\bf {\\large %s} \\\\ %s}\\\\[5mm]" % \
            #     (subj.subject_number, subj.subject_name)
            # print "\\begin{minipage}{80mm}"
            # print "\\end{minipage}"

            print "\\vspace{20mm}"
            print "{\\Large\\bf %s}\\\\[8mm]" % talk.title
            print "{\\Large %s}\\\\[1mm]" % author_long
            print "{%s}\n" % organization
            if logos != "":
                print "{%s}\\\\[10mm]\n" % logos
            else:
                print "\vspace{20mm}\n"
            print "%s\\\\[1mm]" % talk.institute
            print "%s\\\\" % talk.date
            print "\\end{center}"
            print "\\end{frame}\n"

        elif item.kind == "SUBSECTION":
            subsect = item.data
            print "\\subsection{%s}" % subsect.subsect_subsection_name

        elif item.kind == "SLIDE":
            slide = item.data
            prefix = slide.slide_prefix()

            options = ",".join(slide.slide_options)
            if options == "":
                options = "fragile"
            else:
                options = "fragile," + options

            print "\n\\begin{frame}[%s]" % options
            print "\\frametitle{%s%s}" % (prefix, slide.slide_title)
            print_lines(slide.slide_lines, 0, 4)
            print "\\end{frame}\n"

        else:
            unknown_items = unknown_items + [item]

    print "\\end{document}"

    if unknown_items == []:
        status = 0
    else:
        status = 1
        print >> sys.stderr, "transform: internal error (unknown items)"

    return status

###############################################################################

def skip_blank_lines(numbered_lines, num_lines, cur_line_number):
    """
    Given numbered_lines, a list of num_lines NumLines, in which the current
    line given by cur_line_number, return the number of the next nonblank,
    non-comment line.
    """

    while cur_line_number < num_lines:
        line = numbered_lines[cur_line_number].line
        words = line.split()
        if len(words) != 0 and line[0] != "%":
            return cur_line_number
        cur_line_number = cur_line_number + 1
    return cur_line_number

def print_lines(lines, blank_vspace, begin_verb_space):
    """
    Print out the given lines, and make them look good. We do two things to
    make them look good:

    - We ensure that blocks of nonblank lines are separated by a consistent
      amount of white space. If blank_vspace != 0, this will be blank_vspace
      millimetres; otherwise, it will be one latex blank line's worth.

    - If the lines begin with verbatim, we subtract begin_verb_space
      millimetres of vertical space. We need this for targets that insert
      redundant vertical space before verbatim lines, as a counteraction.
    """

    num_lines = len(lines)
    cur_line_number = 0

    in_verbatim = False
    last_space = False
    printed_stuff = False

    while cur_line_number < num_lines:
        line = lines[cur_line_number]

        if line == "\\begin{verbatim}" and begin_verb_space != 0:
            if not printed_stuff:
                print "\\vspace{-%dmm}" % begin_verb_space

        if line == "":
            if in_verbatim:
                print line
            elif not last_space:
                if blank_vspace != 0:
                    print "\\par"
                    print "\\vspace{%dmm}" % blank_vspace
                else:
                    print line
            # Do not print duplicate blank lines outside verbatim.
        else:
            printed_stuff = True
            print line

        if line == "\\begin{verbatim}":
            in_verbatim = True
        elif line == "\\end{verbatim}":
            in_verbatim = False

        if line == "":
            last_space = True
        elif in_verbatim or line[0] != "%":
            last_space = False

        cur_line_number = cur_line_number + 1

###############################################################################

main()
