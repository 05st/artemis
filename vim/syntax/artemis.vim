" Vim syntax file
" Language: Artemis
" Maintainer: 05st
" Latest Revision: 20 August 2021

if exists("b:current_syntax")
    finish
endif

" Keywords
syn keyword Keyword let fn data import namespace mut fnmatch

" Statements
syn keyword Statement if then else match with

" Built-in types
syn keyword Type int float bool char void

" Boolean values
syn keyword Boolean true false

" Integers
syn match Number '[-ox]\?\d\+'

" Floats
syn match Float '[-]\?\d\+\.\d*'

" Strings
syn match SpecialChar contained "\\."
syn region String start='"' end='"' contains=SpecialChar

" Characters
syn match Character "'.'"
syn match Special "'\\.'"


" Operators
syn match Keyword "[:!#$%&*+./<=>\?@^|\-~]\+"

" Semicolons
syn match Keyword ";"

" Identifiers
syn match Ignore "[a-zA-Z][a-zA-Z0-9_']*"

" Comments
syn keyword Todo contained TODO FIXME NOTE
syn match Comment "//.*$" contains=Todo
syn region Comment start="/\*" end="\*/" contains=Todo

