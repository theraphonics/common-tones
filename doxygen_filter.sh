#!/bin/bash

# Regular expressions to match different comment patterns
SINGLE_LINE_COMMENT="^ *;.*$"
MULTI_LINE_COMMENT_START="^ *#\\|.*$"
MULTI_LINE_COMMENT_END="^.*\\|#.*$"
DOCGROUP="^ *;;; @\\(.*\\)$"
TAGS="^ *;;; @[[:alpha:]]+.*$"
MACRO_DEF="^ 
*\(defmacro\|defmacro-.*\)[[:space:]]+([^)]*)[[:space:]]*\(\".*\"\)$"

# Variables to track multi-line comment state
IN_MULTI_LINE_COMMENT=false

# Read from stdin
while IFS= read -r line
do
  # Check if we're inside a multi-line comment
  if $IN_MULTI_LINE_COMMENT; then
    if [[ $line =~ $MULTI_LINE_COMMENT_END ]]; then
      IN_MULTI_LINE_COMMENT=false
    fi
    continue
  fi

  # Check for multi-line comment start
  if [[ $line =~ $MULTI_LINE_COMMENT_START ]]; then
    IN_MULTI_LINE_COMMENT=true
    continue
  fi

  # If the line starts with ";;;", treat it as a Common Lisp comment
  if [[ $line == ";;;"* ]]; then
    # Remove the ";;;" prefix
    line="${line#";;;"}"

    # Check for docstring
    if [[ $line =~ ^[[:space:]]*\".*\" ]]; then
      # Extract the docstring and pass it to Doxygen
      docstring=$(echo "$line" | sed -n 's/^ *\("[^"]*"\).*$/\1/p')
      echo "$docstring"
      continue
    fi

    # Check for special directives
    if [[ $line =~ ^[[:space:]]*#\+.*$ || $line =~ ^[[:space:]]*#-.*$ ]]; 
then
      # Skip lines with special directives
      continue
    fi

    # Check for tags
    if [[ $line =~ $TAGS ]]; then
      # Pass lines with tags to Doxygen as-is
      echo "$line"
      continue
    fi

    # Check for docgroup
    if [[ $line =~ $DOCGROUP ]]; then
      # Remove the docgroup prefix and pass it to Doxygen
      docgroup=$(echo "$line" | sed -n 's/^ *;;; @\(.*\)$/\1/p')
      echo "$docgroup"
      continue
    fi

    # Check for macro definition
    if [[ $line =~ $MACRO_DEF ]]; then
      # Extract the macro name and pass it to Doxygen as a comment
      macro=$(echo "$line" | sed -n 's/^ 
*\(defmacro\|defmacro-.*\)[[:space:]]+([^)]*)[[:space:]]*\(.*\)$/\3/p')
      echo "$macro"
      continue
    fi

    # Pass the line through to Doxygen as a comment
    echo "$line"
  else
    # Pass all other lines through to Doxygen as-is
    echo "$line"
  fi
done

