" Copyright 2009 The Go Authors. All rights reserved.
" Use of this source code is governed by a BSD-style
" license that can be found in the LICENSE file.
"
" cangjie.vim: Vim syntax file for CangJie.
" Language:             CangJie
" Maintainer:           Jerry Wang <jerrywang1981@outlook.com>
" Latest Revision:      2024-06-28
"  2024-06-28:          - initial version
" License:              BSD-style. See LICENSE file in source repository.
" Repository:           https://github.com/jerrywang1981/cj.vim

" Quit when a (custom) syntax file was already loaded
if exists("b:current_syntax")
  finish
endif

let s:keepcpo = &cpo
set cpo&vim

function! s:FoldEnable(...) abort
  if a:0 > 0
    return index(s:FoldEnable(), a:1) > -1
  endif
  return get(g:, 'cj_fold_enable', ['block', 'import', 'from', 'varconst', 'package_comment'])
endfunction

function! s:HighlightArrayWhitespaceError() abort
  return get(g:, 'cj_highlight_array_whitespace_error', 0)
endfunction

function! s:HighlightExtraTypes() abort
  return get(g:, 'cj_highlight_extra_types', 0)
endfunction

function! s:HighlightSpaceTabError() abort
  return get(g:, 'cj_highlight_space_tab_error', 0)
endfunction

function! s:HighlightTrailingWhitespaceError() abort
  return get(g:, 'cj_highlight_trailing_whitespace_error', 0)
endfunction

function! s:HighlightOperators() abort
  return get(g:, 'cj_highlight_operators', 0)
endfunction

function! s:HighlightFunctions() abort
  return get(g:, 'cj_highlight_functions', 0)
endfunction

function! s:HighlightFunctionParameters() abort
  return get(g:, 'cj_highlight_function_parameters', 0)
endfunction

function! s:HighlightFunctionCalls() abort
  return get(g:, 'cj_highlight_function_calls', 0)
endfunction

function! s:HighlightFields() abort
  return get(g:, 'cj_highlight_fields', 0)
endfunction

function! s:HighlightTypes() abort
  return get(g:, 'cj_highlight_types', 0)
endfunction

function! s:HighlightBuildConstraints() abort
  return get(g:, 'cj_highlight_build_constraints', 0)
endfunction

function! s:HighlightStringSpellcheck() abort
  return get(g:, 'cj_highlight_string_spellcheck', 1)
endfunction

function! s:HighlightFormatStrings() abort
  return get(g:, 'cj_highlight_format_strings', 1)
endfunction

function! s:HighlightGenerateTags() abort
  return get(g:, 'cj_highlight_generate_tags', 0)
endfunction

function! s:HighlightVariableAssignments() abort
  return get(g:, 'cj_highlight_variable_assignments', 0)
endfunction

function! s:HighlightVariableDeclarations() abort
  return get(g:, 'cj_highlight_variable_declarations', 0)
endfunction

syn case match

syn keyword     cjPackage           package
syn keyword     cjScopeDecl	        public protected private
syn keyword     cjImport            from import
syn keyword     cjVar               var
syn keyword     cjLet               let
syn keyword     cjConst             const
syn keyword     cjTypedef		        this super


hi def link     cjPackage           Statement
hi def link     cjScopeDecl	        Statement
hi def link     cjTypedef		        Typedef
hi def link     cjImport            Statement
hi def link     cjVar               Keyword
hi def link     cjLet               Keyword
hi def link     cjConst             Keyword
hi def link     cjDeclaration       Keyword

" Keywords within functions
syn keyword     cjStatement         goto return break continue
syn keyword     cjConditional       if else match
syn keyword     cjLabel             case default
syn keyword     cjRepeat            for while

hi def link     cjStatement         Statement
hi def link     cjConditional       Conditional
hi def link     cjLabel             Label
hi def link     cjRepeat            Repeat

" Predefined types
syn keyword     cjType              Bool String
syn keyword     cjSignedInts        Int8 Int16 Int32 Int64 IntNative rune
syn keyword     cjUnsignedInts      UInt8 UInt16 UInt32 UInt64 UIntNative
syn keyword     cjFloats            Float32 Float64

hi def link     cjType              Type
hi def link     cjSignedInts        Type
hi def link     cjUnsignedInts      Type
hi def link     cjFloats            Type

" Predefined functions and values
syn keyword     cjBuiltins                 print
syn keyword     cjBuiltins                 println
syn keyword     cjBoolean                  true false
syn keyword     cjPredefinedIdentifiers    Nothing

hi def link     cjBuiltins                 Identifier
hi def link     cjPredefinedIdentifiers    Constant
" Boolean links to Constant by default by vim: cjBoolean and cjPredefinedIdentifiers
" will be highlighted the same, but having the separate groups allows users to
" have separate highlighting for them if they desire.
hi def link     cjBoolean                  Boolean

" Comments; their contents
syn keyword     cjTodo              contained TODO FIXME XXX BUG
syn cluster     cjCommentGroups      contains=cjTodo

syn region      cjComment           start="//" end="$" contains=cjGenerate,@cjCommentGroups,@Spell
if s:FoldEnable('comment')
  syn region    cjComment           start="/\*" end="\*/" contains=@cjCommentGroups,@Spell fold
  syn match     cjComment           "\v(^\s*//.*\n)+" contains=cjGenerate,@cjCommentGroups,@Spell fold
else
  syn region    cjComment           start="/\*" end="\*/" contains=@cjCommentGroups,@Spell
endif

hi def link     cjComment           Comment
hi def link     cjTodo              Todo

if s:HighlightGenerateTags()
  syn match       cjGenerateVariables contained /\%(\$GOARCH\|\$GOOS\|\$GOFILE\|\$GOLINE\|\$GOPACKAGE\|\$DOLLAR\)\>/
  syn region      cjGenerate          start="^\s*//cjpm:generate" end="$" contains=cjGenerateVariables
  hi def link     cjGenerate          PreProc
  hi def link     cjGenerateVariables Special
endif

syn match       cjEscapeOctal       display contained "\\[0-7]\{3}"
syn match       cjEscapeC           display contained +\\[abfnrtv\\'"]+
syn match       cjEscapeX           display contained "\\x\x\{2}"
syn match       cjEscapeU           display contained "\\u\x\{4}"
syn match       cjEscapeBigU        display contained "\\U\x\{8}"
syn match       cjEscapeError       display contained +\\[^0-7xuUabfnrtv\\'"]+

hi def link     cjEscapeOctal       cjSpecialString
hi def link     cjEscapeC           cjSpecialString
hi def link     cjEscapeX           cjSpecialString
hi def link     cjEscapeU           cjSpecialString
hi def link     cjEscapeBigU        cjSpecialString
hi def link     cjSpecialString     Special
hi def link     cjEscapeError       Error

" Strings and their contents
syn cluster     cjStringGroup       contains=cjEscapeOctal,cjEscapeC,cjEscapeX,cjEscapeU,cjEscapeBigU,cjEscapeError
if s:HighlightStringSpellcheck()
  syn region      cjString            start=+"+ skip=+\\\\\|\\"+ end=+"+ contains=@cjStringGroup,@Spell
  syn region      cjRawString         start=+`+ end=+`+ contains=@Spell
else
  syn region      cjString            start=+"+ skip=+\\\\\|\\"+ end=+"+ contains=@cjStringGroup
  syn region      cjRawString         start=+`+ end=+`+
endif

syn match       cjImportString      /^\%(\s\+\|import \)\(\h\w* \)\?\zs"[^"]\+"/ contained containedin=cjImport

if s:HighlightFormatStrings()
  " [n] notation is valid for specifying explicit argument indexes
  " 1. Match a literal % not preceded by a %.
  " 2. Match any number of -, #, 0, space, or +
  " 3. Match * or [n]* or any number or nothing before a .
  " 4. Match * or [n]* or any number or nothing after a .
  " 5. Match [n] or nothing before a verb
  " 6. Match a formatting verb
  syn match       cjFormatSpecifier   /\
        \%([^%]\%(%%\)*\)\
        \@<=%[-#0 +]*\
        \%(\%(\%(\[\d\+\]\)\=\*\)\|\d\+\)\=\
        \%(\.\%(\%(\%(\[\d\+\]\)\=\*\)\|\d\+\)\=\)\=\
        \%(\[\d\+\]\)\=[vTtbcdoqxXUeEfFgGspw]/ contained containedin=cjString,cjRawString
  hi def link     cjFormatSpecifier   cjSpecialString
endif

hi def link     cjImportString      String
hi def link     cjString            String
hi def link     cjRawString         String

" Characters; their contents
syn cluster     cjCharacterGroup    contains=cjEscapeOctal,cjEscapeC,cjEscapeX,cjEscapeU,cjEscapeBigU
syn region      cjCharacter         start=+'+ skip=+\\\\\|\\'+ end=+'+ contains=@cjCharacterGroup

hi def link     cjCharacter         Character

" Regions
syn region      cjParen             start='(' end=')' transparent
if s:FoldEnable('block')
  syn region    cjBlock             start="{" end="}" transparent fold
else
  syn region    cjBlock             start="{" end="}" transparent
endif

" var, const
if s:FoldEnable('varconst')
  syn match     cjVar               /var ()/ transparent fold
                                  \ contains=cjVar
  syn region    cjConst             start='const (' end='^\s*)$' transparent fold
                                  \ contains=ALLBUT,cjParen,cjBlock,cjFunction,cjTypeName,cjParamName,cjParamType,cjSimpleParams,cjPointerOperator

else
  syn region    cjVar               start='var ('   end='^\s*)$' transparent
                                  \ contains=ALLBUT,cjParen,cjBlock,cjFunction,cjTypeName,cjParamName,cjParamType,cjSimpleParams,cjPointerOperator
  syn match     cjVar               /var ()/ transparent
                                  \ contains=cjVar
  syn region    cjConst             start='const (' end='^\s*)$' transparent
                                  \ contains=ALLBUT,cjParen,cjBlock,cjFunction,cjTypeName,cjParamName,cjParamType,cjSimpleParams,cjPointerOperator
  syn match     cjConst             /const ()/ transparent
                                  \ contains=cjConst
endif

" Single-line var, const, and import.
syn match       cjSingleDecl        /\%(import\|var\|const\) [^(]\@=/ contains=cjImport,cjVar,cjConst

" Integers
syn match       cjDecimalInt        "\<-\=\%(0\|\%(\d\|\d_\d\)\+\)\>"
syn match       cjHexadecimalInt    "\<-\=0[xX]_\?\%(\x\|\x_\x\)\+\>"
syn match       cjOctalInt          "\<-\=0[oO]\?_\?\%(\o\|\o_\o\)\+\>"
syn match       cjBinaryInt         "\<-\=0[bB]_\?\%([01]\|[01]_[01]\)\+\>"

hi def link     cjDecimalInt        Integer
hi def link     cjDecimalError      Error
hi def link     cjHexadecimalInt    Integer
hi def link     cjHexadecimalError  Error
hi def link     cjOctalInt          Integer
hi def link     cjOctalError        Error
hi def link     cjBinaryInt         Integer
hi def link     cjBinaryError       Error
hi def link     Integer             Number

" Floating point
"float_lit         = decimal_float_lit | hex_float_lit .
"
"decimal_float_lit = decimal_digits "." [ decimal_digits ] [ decimal_exponent ] |
"                    decimal_digits decimal_exponent |
"                    "." decimal_digits [ decimal_exponent ] .
"decimal_exponent  = ( "e" | "E" ) [ "+" | "-" ] decimal_digits .
"
"hex_float_lit     = "0" ( "x" | "X" ) hex_mantissa hex_exponent .
"hex_mantissa      = [ "_" ] hex_digits "." [ hex_digits ] |
"                    [ "_" ] hex_digits |
"                    "." hex_digits .
"hex_exponent      = ( "p" | "P" ) [ "+" | "-" ] decimal_digits .
" decimal floats with a decimal point
syn match       cjFloat             "\<-\=\%(0\|\%(\d\|\d_\d\)\+\)\.\%(\%(\%(\d\|\d_\d\)\+\)\=\%([Ee][-+]\=\%(\d\|\d_\d\)\+\)\=\>\)\="
syn match       cjFloat             "\s\zs-\=\.\%(\d\|\d_\d\)\+\%(\%([Ee][-+]\=\%(\d\|\d_\d\)\+\)\>\)\="
" decimal floats without a decimal point
syn match       cjFloat             "\<-\=\%(0\|\%(\d\|\d_\d\)\+\)[Ee][-+]\=\%(\d\|\d_\d\)\+\>"
" hexadecimal floats with a decimal point
syn match       cjHexadecimalFloat  "\<-\=0[xX]\%(_\x\|\x\)\+\.\%(\%(\x\|\x_\x\)\+\)\=\%([Pp][-+]\=\%(\d\|\d_\d\)\+\)\=\>"
syn match       cjHexadecimalFloat  "\<-\=0[xX]\.\%(\x\|\x_\x\)\+\%([Pp][-+]\=\%(\d\|\d_\d\)\+\)\=\>"
" hexadecimal floats without a decimal point
syn match       cjHexadecimalFloat  "\<-\=0[xX]\%(_\x\|\x\)\+[Pp][-+]\=\%(\d\|\d_\d\)\+\>"

hi def link     cjFloat             Float
hi def link     cjHexadecimalFloat  Float

" Spaces after "[]"
if s:HighlightArrayWhitespaceError()
  syn match cjSpaceError display "\%(\[\]\)\@<=\s\+"
endif

" Space-tab error
if s:HighlightSpaceTabError()
  syn match cjSpaceError display " \+\t"me=e-1
endif

" Trailing white space error
if s:HighlightTrailingWhitespaceError()
  syn match cjSpaceError display excludenl "\s\+$"
endif

hi def link     cjSpaceError        Error



" Comments; their contents
syn keyword     cjTodo              contained NOTE
hi def link     cjTodo              Todo

syn match cjVarArgs /\.\.\./

" Operators;
if s:HighlightOperators()
  " match single-char operators:          - + % < > ! & | ^ * =
  " and corresponding two-char operators: -= += %= <= >= != &= |= ^= *= ==
  syn match cjOperator /[-+%<>!&|^*=]=\?/
  " match / and /=
  syn match cjOperator /\/\%(=\|\ze[^/*]\)/
  " match two-char operators:               << >> &^
  " and corresponding three-char operators: <<= >>= &^=
  syn match cjOperator /\%(<<\|>>\|&^\)=\?/
  " match remaining two-char operators: := && || <- ++ --
  syn match cjOperator /:=\|||\|<-\|++\|--/
  " match ~
  syn match cjOperator /\~/
  " match ...

  hi def link     cjPointerOperator   cjOperator
  hi def link     cjVarArgs           cjOperator
endif
hi def link     cjOperator          Operator

syn match cjTypeParams        /\[\%(\w\+\s\+\%(\~\?\%(\[]\)\?\w\%(\w\||\)\)*\%(,\s*\)\?\)\+\]/ nextgroup=cjSimpleParams,cjDeclType contained

" Functions;
if s:HighlightFunctions() || s:HighlightFunctionParameters()
  syn match cjDeclaration       /\<func\>/ nextgroup=cjFunction,cjSimpleParams skipwhite skipnl
  syn match cjFunction          /\w\+/ nextgroup=cjSimpleParams,cjTypeParams contained skipwhite skipnl
  if s:HighlightFunctionParameters()
    syn match cjSimpleParams      /(\%(\w\|\_s\|[*\.\[\],\{\}<>-]\)*)/ contained contains=cjParamName,cjType nextgroup=cjFunctionReturn skipwhite skipnl
    syn match cjFunctionReturn   /(\%(\w\|\_s\|[*\.\[\],\{\}<>-]\)*)/ contained contains=cjParamName,cjType skipwhite skipnl
    syn match cjParamName        /\w\+\%(\s*,\s*\w\+\)*\ze\s\+\%(\w\|\.\|\*\|\[\)/ contained nextgroup=cjParamType skipwhite skipnl
    syn match cjParamType        /\%([^,)]\|\_s\)\+,\?/ contained nextgroup=cjParamName skipwhite skipnl
                          \ contains=cjVarArgs,cjType,cjSignedInts,cjUnsignedInts,cjFloats,cjDeclType,cjBlock
    hi def link   cjParamName      Identifier
  endif
else
  syn keyword cjDeclaration func main
endif
hi def link     cjFunction          Function

" Function calls;
if s:HighlightFunctionCalls()
  syn match cjFunctionCall      /\w\+\ze\%(\[\%(\%(\[]\)\?\w\+\(,\s*\)\?\)\+\]\)\?(/ contains=cjBuiltins,cjDeclaration
endif
hi def link     cjFunctionCall      Type

" Fields;
if s:HighlightFields()
  syn match       cjField   /\.\w\+\
        \%(\%([\/\-\+*%]\)\|\
        \%([\[\]{}<\>\)]\)\|\
        \%([\!=\^|&]\)\|\
        \%([\n\r\ ]\)\|\
        \%([,\:.]\)\)\@=/hs=s+1
endif
hi def link    cjField              Identifier

" Structs & Interfaces;
if s:HighlightTypes()
  syn match cjTypeConstructor      /\<\w\+{\@=/
  syn match cjTypeDecl             /\<type\>/ nextgroup=cjTypeName skipwhite skipnl
  syn match cjTypeName             /\w\+/ contained nextgroup=cjDeclType,cjTypeParams skipwhite skipnl
  syn match cjDeclType             /\<\%(interface\|struct\)\>/ skipwhite skipnl
else
  syn keyword cjDeclType           struct interface
  syn keyword cjDeclaration        type
endif
hi def link     cjTypeConstructor   Type
hi def link     cjTypeName          Type
hi def link     cjTypeDecl          Keyword
hi def link     cjDeclType          Keyword

" Variable Assignments
if s:HighlightVariableAssignments()
  syn match cjVarAssign /\v[_.[:alnum:]]+(,\s*[_.[:alnum:]]+)*\ze(\s*([-^+|^\/%&]|\*|\<\<|\>\>|\&\^)?\=[^=])/
  hi def link   cjVarAssign         Special
endif

" Variable Declarations
if s:HighlightVariableDeclarations()
  syn match cjVarDefs /\v\w+(,\s*\w+)*\ze(\s*:\=)/
  hi def link   cjVarDefs           Special
endif

" Build Constraints
if s:HighlightBuildConstraints()
  syn match   cjBuildKeyword      display contained "+build\|go:build"
  syn keyword cjBuildDirectives   contained
        \ android darwin dragonfly freebsd linux nacl netbsd openbsd plan9
        \ solaris windows 386 amd64 amd64p32 arm armbe arm64 arm64be ppc64
        \ ppc64le mips mipsle mips64 mips64le mips64p32 mips64p32le ppc
        \ s390 s390x sparc sparc64 cgo ignore race

  " Other words in the build directive are build tags not listed above, so
  " avoid highlighting them as comments by using a matchgroup just for the
  " start of the comment.
  " The rs=s+2 option lets the \s*+build portion be part of the inner region
  " instead of the matchgroup so it will be highlighted as a cjBuildKeyword.
  syn region  cjBuildComment      matchgroup=cjBuildCommentStart
        \ start="//\(\s*+build\s\|go:build\)"rs=s+2 end="$"
        \ contains=cjBuildKeyword,cjBuildDirectives
  hi def link cjBuildCommentStart Comment
  hi def link cjBuildDirectives   Type
  hi def link cjBuildKeyword      PreProc
endif

if s:HighlightBuildConstraints() || s:FoldEnable('package_comment')
  " One or more line comments that are followed immediately by a "package"
  " declaration are treated like package documentation, so these must be
  " matched as comments to avoid looking like working build constraints.
  " The he, me, and re options let the "package" itself be highlighted by
  " the usual rules.
  exe 'syn region  cjPackageComment    start=/\v(\/\/.*\n)+\s*package\s/'
        \ . ' end=/\v\n\s*package\s/he=e-8,me=e-8,re=e-8'
        \ . ' contains=@cjCommentGroups,@Spell'
        \ . (s:FoldEnable('package_comment') ? ' fold' : '')
  exe 'syn region  cjPackageComment    start=/\v^\s*\/\*.*\n(.*\n)*\s*\*\/\npackage\s/'
        \ . ' end=/\v\*\/\n\s*package\s/he=e-8,me=e-8,re=e-8'
        \ . ' contains=@cjCommentGroups,@Spell'
        \ . (s:FoldEnable('package_comment') ? ' fold' : '')
  hi def link cjPackageComment    Comment
endif

let b:current_syntax = "cangjie"

let &cpo = s:keepcpo
unlet s:keepcpo

" vim: sw=2 sts=2 et

