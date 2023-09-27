" waikiki autoload
let s:save_cpo = &cpo
set cpo&vim

"------------
" Debug {{{1
"------------
let g:waikiki_debug = 1
if 0
append
  " comment out all dbg calls
  :g,\c^\s*call <Sid>Dbg(,s/call/"call/
  " uncomment
  :g,\c^\s*"call <Sid>Dbg(,s/"call/call/
.
endif


"----------------
" Variables {{{1
"----------------

let s:dirsep = get(g:, 'waikiki_dirsep', '/')
let s:follow = get(g:, 'waikiki_follow_action', 'edit')
let s:create = get(g:, 'waikiki_create_action', 'edit')
let s:ext    = get(g:, 'waikiki_ext', '.md')
let s:index  = get(g:, 'waikiki_index', 'index'.s:ext)
let s:wiki_roots     = get(g:, 'waikiki_wiki_roots',
                        \ get(g:, 'waikiki_roots', []))
let s:lookup_order   = get(g:, 'waikiki_lookup_order', ['raw', 'ext', 'subdir'])
let s:mkdir_prompt   = get(g:, 'waikiki_mkdir_prompt', 0)
let s:ask_if_noindex = get(g:, 'waikiki_ask_if_noindex', 0)
let s:create_type    = get(g:, 'waikiki_create_type', 'ext')
let s:space_replacement = get(g:, 'waikiki_space_replacement', '_')


"-----------------------
" Public Functions {{{1
"-----------------------

" waikiki#NextLink {{{2
function! waikiki#NextLink() abort
  let link_regex = get(g:, 'waikiki_link_regex', '\[[^]]*\]([^)]\+)')
  call search(link_regex)
endfun

" waikiki#PrevLink {{{2
function! waikiki#PrevLink() abort
  let link_regex = get(g:, 'waikiki_link_regex', '\[[^]]*\]([^)]\+)')
  call search(link_regex, 'b')
endfun


" waikiki#SetupBuffer {{{2
function! waikiki#SetupBuffer() abort
  call <Sid>Dbg("Setting up buffer")

  setl concealcursor=n

  if exists('#Waikiki#User#setup')
    call <Sid>Dbg("doauto Waikiki User setup")
    doauto <nomodeline> Waikiki User setup
  endif
endfun

" waikiki#CheckBuffer {{{2
function! waikiki#CheckBuffer(file) abort
  if s:IsUnderWikiRoot(a:file)
    call waikiki#SetupBuffer()
    return
  endif
  call <Sid>Dbg("nothing to setup")
endfun

" waikiki#GetCurrentLink {{{2
function! waikiki#GetCurrentLink() abort
  let link_url_regex = get(g:, 'waikiki_link_url_regex',
        \ '\[[^]]*\](\zs[^)]\+\ze)' )
  let line = getline('.')
  let link = matchstr(line,
        \ '\%<'.(col('.')+1).'c'.
        \ link_url_regex.
        \ '\%>'.col('.').'c')
  call <Sid>Dbg("Current link:", link)
  return link
endfun

" waikiki#GetCurrentWord {{{2
function! waikiki#GetCurrentWord() abort
  let word_regex = get(g:, 'waikiki_word_regex',
        \ '[-+0-9A-Za-z_]\+' )
  let line = getline('.')
  let word = matchstr(line,
        \ '\%<'.(col('.')+1).'c'.
        \ word_regex.
        \ '\%>'.col('.').'c')
  call <Sid>Dbg("Current word:", link)
  return word
endfun

" waikiki#FollowLink {{{2
function! waikiki#FollowLink(...) abort
	if ! s:IsUnderWikiRoot(expand('<afile>:p'))
		return
	endif
	if ! exists("g:mwikiEnterLinkStack")
		let g:mwikiEnterLinkStack = []
	endif
	let g:mwikiEnterLinkStack = add(g:mwikiEnterLinkStack, expand("%:p")."@pos".join(getpos("."),","))

  let options = a:0 ? a:1 : {}
  let follow  = get(options, 'action', s:follow)
  let create  = get(options, 'create', s:create)
  let name    = get(options, 'name',
                  \ get(g:, 'waikiki_use_word_regex', 0)
                  \   ? waikiki#GetCurrentWord()
                  \   : expand('<cword>'))
  let curlink = get(options, 'link', waikiki#GetCurrentLink())
  let curpath = expand('%:p:h')
  let targetlist  = []
  let finaltarget = ''
  call <Sid>Dbg("name, link: ", name, curlink)

  " is there a link with a url
  if curlink != ""
    " yes, got a link
    let link_info = s:GetTargetInfo(curlink)
    " does it have a path component
    if link_info['has_path']
      let abstarget = s:JoinPath(curpath, curlink)
      let finaltarget = isdirectory(abstarget)
            \ ? s:JoinPath(abstarget, s:index)
            \ : abstarget
      call <Sid>Dbg("link with path: ", finaltarget)
      if filereadable(finaltarget)
        exe follow finaltarget
        return
      elseif !link_info['has_ext'] && filereadable(finaltarget.s:ext)
        exe follow finaltarget.s:ext
        return
      endif
    else
      " no path, look up file in expected locations
      let targetlist = s:GetPossibleTargetsOrderedList(curlink)
      for target in targetlist
        let abstarget = s:JoinPath(curpath, target)
        call <Sid>Dbg("trying: ", abstarget)
        if filereadable(abstarget)
          exe follow abstarget
          return
        endif
      endfor
      call <Sid>Dbg("all failed.")
    endif
  endif

  " cannot find page, let's create one

  " set url if we don't have it yet
  if finaltarget == ''
    " get target
    let targetbase = (curlink != "" ? curlink : name)
    if s:create_type != ''
      " user has prefs, don't prompt
      let targetdict = s:GetPossibleTargetsDict(targetbase)
      let target = get(targetdict, s:create_type, targetdict['raw'])
    else
      if empty(targetlist)
        let targetlist = s:GetPossibleTargetsOrderedList(targetbase)
      endif
      let target = s:PromptForTarget(targetlist)
    endif
    let nospacetarget = substitute(target, ' ', s:space_replacement, 'g')
    let finaltarget = s:JoinPath(curpath, nospacetarget)
    call <Sid>Dbg("nospacetarget, finaltarget:", nospacetarget, finaltarget)
    if curlink == ""
      call s:InsertLinkCode(name, nospacetarget)
    endif
  endif

  call s:EnsurePathExists(finaltarget)
  exe create finaltarget
endfun

" waikiki#GoUp {{{2
function! waikiki#GoUp(...) abort
  let options     = a:0 ? a:1 : {}
  let action      = get(options, 'action', s:follow)
    if exists("g:mwikiEnterLinkStack") && len(g:mwikiEnterLinkStack) != 0
        let res = split(g:mwikiEnterLinkStack[len(g:mwikiEnterLinkStack)-1], "@pos")
        execute "edit ".res[0]
        call setpos(".",split(res[1], ","))
        unlet g:mwikiEnterLinkStack[len(g:mwikiEnterLinkStack)-1]
    else
      echo "Already at wiki root."
    endif
endfun

"------------------------
" Private Functions {{{1
"------------------------

" s:GetPossibleTargetsDict {{{2
function! s:GetPossibleTargetsDict(target) abort
  let targetsdict_func = get(g:, 'waikiki_targetsdict_func', '')
  let target_info = s:GetTargetInfo(a:target)
  let ret = targetsdict_func != '' ? function(targetsdict_func)(a:target) : {}
  let ret['raw']    = a:target
  let ret['ext']    = a:target . (target_info['has_ext'] ? '' : s:ext)
  let ret['subdir'] = a:target . s:dirsep . s:index
  return ret
endfun

" s:GetPossibleTargetsOrderedList {{{2
function! s:GetPossibleTargetsOrderedList(name) abort
  let targetlist = []
  let targetdict = s:GetPossibleTargetsDict(a:name)
  for type in s:lookup_order
    let target = get(targetdict, type, '')
    call add(targetlist, target)
  endfor
  call <Sid>Dbg("Target list:", string(targetlist))
  return targetlist
endfun

" s:GetTargetInfo {{{2
function! s:GetTargetInfo(target) abort
  let tlen = strlen(a:target)
  let elen = strlen(s:ext)
  let ret = {}
  let ret['has_path'] = (stridx(a:target, s:dirsep) != -1)
  let ret['has_ext']  = ((tlen > elen)
        \ && (stridx(a:target, s:ext) == (tlen - elen)))
  return ret
endfun

" s:PromptForTarget {{{2
function! s:PromptForTarget(choices, ...) abort
  let options  = a:0 ? a:1 : {}
  let prompt   = get(options, 'prompt', "Choose new file path:")
  let complete = get(options, 'complete', 0)
  let target   = ''
  while target == ''
    echo prompt
    let i = 1
    for target in a:choices
      echo printf("%d) %s", i, target)
      let i += 1
    endfor
    let last_idx = i
    echo printf("%d) %s", i, "[other]")
    let choice = input('> ')
    let choice_nr = str2nr(choice)
    if choice_nr >= 1 && choice_nr < last_idx
      let target = a:choices[choice_nr-1]
    elseif choice_nr == last_idx
      " User enters path
      let target = complete
            \ ? input('path: ', expand('%:h'), "file")
            \ : input('path: ')
    endif
  endwhile
  call <Sid>Dbg("Chosen target:", target)
  return target
endfun

" s:EnsurePathExists {{{2
function! s:EnsurePathExists(target) abort
  let path = matchstr(a:target, '.*'.s:dirsep)
  if path != '' && !isdirectory(path)
    if s:mkdir_prompt
      let reply = ''
      while reply !~ 'y\%[es]\c' && reply !~ 'n\%[o]\c'
        echo "create dir(s) '".path."'? [y/n]: "
      endwhile
      if reply =~ 'y\%[es]\c'
        call mkdir(path, 'p')
      else
        echom "Warning: new buffer path won't exist."
      endif
    else
      call mkdir(path, 'p')
    endif
  endif
endfun

" s:InsertLinkCode {{{2
function! s:InsertLinkCode(name, target) abort
  " TODO: test and improve escaping?
  let escaped_name = escape(a:name, '\*^$')
  let repl_fmt     = get(g:, 'waikiki_link_fmt', '[%s](%s)%.0s')
  let is_md_link   = (len(repl_fmt) > 4 && repl_fmt[0:3] is '[%s]')
  let replacement  = printf(repl_fmt, a:name, a:target, a:name)
  let line = substitute(getline('.'),
        \ '\%<'.(col('.')+1).'c'.
        \   (is_md_link ? '\[\?' : '').
        \ escaped_name.
        \ '\%>'.col('.').'c'.
        \   (is_md_link ? '\]\?' : ''),
        \ replacement,
        \ '')
  call setline('.', line)
endfun

" s:JoinPath {{{2
function! s:JoinPath(path, file) abort
  if a:path[strlen(a:path)-1] == s:dirsep
        \ || a:file[0] == s:dirsep
    return a:path . a:file
  else
    return a:path . s:dirsep . a:file
  endif
endfun

" s:IsSubdirOf {{{2
function! s:IsSubdirOf(subdir, parent) abort
  " normalized paths
  let nsubdir   = s:ChompDirSep(a:subdir).s:dirsep
  let nparent   = s:ChompDirSep(a:parent).s:dirsep
  let subdircut = strcharpart(nsubdir, 0, strchars(nparent))
  let is_subdir = (subdircut == nparent)
  call <Sid>Dbg("is subdir of:", subdircut, nparent, (is_subdir?"yes":"no"))
  return is_subdir
endfun

" s:IsAtDir {{{2
function! s:IsAtDir(dir1, dir2) abort
  " normalized paths
  let ndir1 = s:ChompDirSep(a:dir1).s:dirsep
  let ndir2 = s:ChompDirSep(a:dir2).s:dirsep
  let is_at_dir = (ndir1 == ndir2)
  call <Sid>Dbg("is at dir:", ndir1, ndir2, (is_at_dir?"yes":"no"))
  return is_at_dir
endfun


" s:GetBufferWikiRoot {{{2
function! s:GetBufferWikiRoot(file) abort
  let abspath = fnamemodify(a:file, ':p:h')
  for root in s:wiki_roots
    let absroot = fnamemodify(root, ':p')
    if s:IsSubdirOf(abspath, absroot)
      return absroot
    endif
  endfor
  return ""
endfun

" s:IsUnderWikiRoot {{{2
function! s:IsUnderWikiRoot(file) abort
  return (s:GetBufferWikiRoot(a:file) != "")
endfun

" s:IsAtWikiRoot {{{2
function! s:IsPathAtWikiRoot(path) abort
  for root in s:wiki_roots
    let absroot = fnamemodify(root, ':p')
    if s:IsAtDir(a:path, absroot)
      return 1
    endif
  endfor
  return 0
endfun

" s:ChompDirSep {{{2
function! s:ChompDirSep(str) abort
  let l = strchars(a:str)
  let ret = a:str
  if strcharpart(a:str, l -1, 1) == s:dirsep
    let ret = strcharpart(a:str, 0, l - 1)
  endif
  return ret
endfun

" s:Dbg {{{2
function! s:Dbg(msg, ...) abort
  if g:waikiki_debug
    let m = a:msg
    if a:0
      let m .= " [".join(a:000, "] [")."]"
    endif
    echom m
  endif
endfun


let &cpo = s:save_cpo
