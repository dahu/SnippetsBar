"" ============================================================================
" File:        SnippetsBar.vim
" Description: List the current file's snippets in a sidebar, a la Tagbar
" Author:      Barry Arthur <barry.arthur@gmail.com>
" Licence:     Vim licence
" Website:     http://dahu.github.com/SnippetsBar/
" Version:     0.1
" Note:        This plugin was heavily inspired by the 'Tagbar' plugin by
"              Jan Larres and uses great gobs of code from it.
"
" Original taglist copyright notice:
"              Permission is hereby granted to use and distribute this code,
"              with or without modifications, provided that this copyright
"              notice is copied with it. Like anything else that's free,
"              taglist.vim is provided *as is* and comes with no warranty of
"              any kind, either expressed or implied. In no event will the
"              copyright holder be liable for any damamges resulting from the
"              use of this software.
" ============================================================================

if &cp || exists('g:loaded_snippetsbar')
  finish
endif

" Initialization {{{1

" Basic init {{{2

if v:version < 700
  echomsg 'SnippetsBar: Vim version is too old, SnippetsBar requires at least 7.0'
  finish
endif

redir => s:ftype_out
silent filetype
redir END
if s:ftype_out !~# 'detection:ON'
  echomsg 'SnippetsBar: Filetype detection is turned off, skipping plugin'
  unlet s:ftype_out
  finish
endif
unlet s:ftype_out

let g:loaded_snippetsbar = 1

if !exists('g:snippetsbar_left')
  let g:snippetsbar_left = 0
endif

if !exists('g:snippetsbar_width')
  let g:snippetsbar_width = 10
endif

if !exists('g:snippetsbar_autoclose')
  let g:snippetsbar_autoclose = 0
endif

if !exists('g:snippetsbar_compact')
    let g:snippetsbar_compact = 0
endif

if !exists('g:snippetsbar_expand')
  let g:snippetsbar_expand = 0
endif

let s:snippets_init_done    = 0
let s:autocommands_done     = 0
let s:window_expanded       = 0
let s:snippets              = []

" From :help <SID>
function s:SID()
  return matchstr(expand('<sfile>'), '<SNR>\d\+_\zeSID$')
endfun

let s:engines = {
      \ 'snipmate': {
      \   'detect': 'exists("g:loaded_snips")',
      \   'list_func': 'snipMate#GetSnippetsForWordBelowCursor',
      \   'list_func_args': 'a:word,"*",0',
      \   'list_map': 'v:val[0]'
      \   },
      \ 'xptemplate': {
      \   'detect': 'exists(''g:__XPTEMPLATE_VIM__'')',
      \   'list_func': s:SID().'xptemplate_helper',
      \   'list_func_args': 'a:word'
      \   },
      \ 'ultisnips': {
      \   'detect': 'exists(''g:did_UltiSnips_vim'')',
      \   'list_func': s:SID().'ultisnips_helper',
      \   'list_func_args': 'a:word'
      \   }
      \ }

" TODO: Should we allow only one engine?
if exists('g:snippetsbar_engines')
  " Add extra entries
  call extend(s:engines, g:snippetsbar_engines, 'keep')
  " Merge common entries
  call map(s:engines,
        \ 'has_key(g:snippetsbar_engines, v:key)'.
        \ '? extend(s:engines[v:key], g:snippetsbar_engines[v:key], "force")'.
        \ ': v:val')
endif

" s:CreateAutocommands() {{{2
function! s:CreateAutocommands()
    augroup SnippetsBarAutoCmds
        autocmd!
        autocmd BufEnter   __SnippetsBar__ nested call s:QuitIfOnlyWindow()
        autocmd BufUnload  __SnippetsBar__ call s:CleanUp()
        "autocmd CursorHold __SnippetsBar__ call s:ShowSnippetExpansion()
        autocmd FileType * unlet! b:did_xpt

        autocmd CursorMovedI,InsertEnter * call
                    \ s:AutoUpdate()
        autocmd BufDelete * call
                    \ s:CleanupFileinfo(fnamemodify(expand('<afile>'), ':p'))
    augroup END

    let s:autocommands_done = 1
endfunction

" s:MapKeys() {{{2
function! s:MapKeys()
  "nnoremap <script> <silent> <buffer> <CR>
        "\ :call <SID>InsertSnippet()<CR>
  "nnoremap <script> <silent> <buffer> <2-LeftMouse>
        "\ :call <SID>InsertSnippet()<CR>
  "nnoremap <script> <silent> <buffer> <LeftRelease> <LeftRelease>
        "\ :call <SID>CheckMouseClick()<CR>
  "nnoremap <script> <silent> <buffer> <Space>
        "\ :call <SID>ShowSnippetExpansion()<CR>

  nnoremap <script> <silent> <buffer> q    :call <SID>CloseWindow()<CR>
endfunction


" Window management {{{1
" Window management code shamelessly stolen from the Tagbar plugin:
" http://www.vim.org/scripts/script.php?script_id=3465

" s:ToggleWindow() {{{2
function! s:ToggleWindow()
  let snippetsbarwinnr = bufwinnr("__SnippetsBar__")
  if snippetsbarwinnr != -1
    call s:CloseWindow()
    return
  endif

  call s:OpenWindow(0)
endfunction

" s:OpenWindow() {{{2
function! s:OpenWindow(autoclose)
  call s:set_engine()
  if !exists('s:engine')
    " XXX There is nothing to show.
    echohl ErrorMsg
    echom 'SnippetsBar could not find a snippets engine!'
    echohl None
    return
  endif
  " If the snippetsbar window is already open jump to it
  let snippetsbarwinnr = bufwinnr('__SnippetsBar__')
  if snippetsbarwinnr != -1
    if winnr() != snippetsbarwinnr
      execute snippetsbarwinnr . 'wincmd w'
    endif
    return
  endif

  "if !s:snippets_init_done
  "call s:InitSnippets()
  "endif

  " Expand the Vim window to accomodate for the SnippetsBar window if requested
  if g:snippetsbar_expand && !s:window_expanded && has('gui_running')
    let &columns += g:snippetsbar_width + 1
    let s:window_expanded = 1
  endif

  let openpos = g:snippetsbar_left ? 'topleft vertical ' : 'botright vertical '
  exe 'silent keepalt ' . openpos . g:snippetsbar_width . 'split ' . '__SnippetsBar__'

  call s:InitWindow(a:autoclose)

  execute 'wincmd p'

  "" Jump back to the snippetsbar window if autoclose or autofocus is set. Can't
  "" just stay in it since it wouldn't trigger the update event
  "if g:snippetsbar_autoclose || a:autoclose || g:snippetsbar_autofocus
  "let snippetsbarwinnr = bufwinnr('__SnippetsBar__')
  "execute snippetsbarwinnr . 'wincmd w'
  "endif
endfunction

" s:InitWindow() {{{2
function! s:InitWindow(autoclose)
  setlocal noreadonly " in case the "view" mode is used
  setlocal buftype=nofile
  setlocal bufhidden=hide
  setlocal noswapfile
  setlocal nobuflisted
  setlocal nomodifiable
  setlocal filetype=snippetsbar
  setlocal nolist
  setlocal nonumber
  setlocal nowrap
  setlocal winfixwidth
  setlocal textwidth=0

  if exists('+relativenumber')
    setlocal norelativenumber
  endif

  setlocal nofoldenable
  setlocal foldcolumn=0
  " Reset fold settings in case a plugin set them globally to something
  " expensive. Apparently 'foldexpr' gets executed even if 'foldenable' is
  " off, and then for every appended line (like with :put).
  setlocal foldmethod&
  setlocal foldexpr&

  "setlocal statusline=%!SnippetsBarGenerateStatusline()

  " Script-local variable needed since compare functions can't
  " take extra arguments
  let s:compare_typeinfo = {}

  let s:is_maximized = 0

  let w:autoclose = a:autoclose

  "if has('balloon_eval')
  "setlocal balloonexpr=SnippetsBarBalloonExpr()
  "set ballooneval
  "endif

  let cpoptions_save = &cpoptions
  set cpoptions&vim

  "if !hasmapto('JumpToTag', 'n')
  "call s:MapKeys()
  "endif

  if !s:autocommands_done
    call s:CreateAutocommands()
  endif

  let &cpoptions = cpoptions_save
endfunction

" s:CloseWindow() {{{2
function! s:CloseWindow()
  let snippetsbarwinnr = bufwinnr('__SnippetsBar__')
  if snippetsbarwinnr == -1
    return
  endif

  let snippetsbarbufnr = winbufnr(snippetsbarwinnr)

  if winnr() == snippetsbarwinnr
    if winbufnr(2) != -1
      " Other windows are open, only close the snippetsbar one
      close
    endif
  else
    " Go to the snippetsbar window, close it and then come back to the
    " original window
    let curbufnr = bufnr('%')
    execute snippetsbarwinnr . 'wincmd w'
    close
    " Need to jump back to the original window only if we are not
    " already in that window
    let winnum = bufwinnr(curbufnr)
    if winnr() != winnum
      exe winnum . 'wincmd w'
    endif
  endif

  " If the Vim window has been expanded, and SnippetsBar is not open in any other
  " tabpages, shrink the window again
  if s:window_expanded
    let tablist = []
    for i in range(tabpagenr('$'))
      call extend(tablist, tabpagebuflist(i + 1))
    endfor

    if index(tablist, snippetsbarbufnr) == -1
      let &columns -= g:snippetsbar_width + 1
      let s:window_expanded = 0
    endif
  endif
endfunction

" s:ZoomWindow() {{{2
function! s:ZoomWindow()
  if s:is_maximized
    execute 'vert resize ' . g:snippetsbar_width
    let s:is_maximized = 0
  else
    vert resize
    let s:is_maximized = 1
  endif
endfunction


" Display {{{1
" s:RenderContent() {{{2
"function! s:RenderContent(...)
function! s:RenderContent(word)
  "if a:0 == 1
    "let fileinfo = a:1
  "else
    "let fileinfo = s:known_files.getCurrent()
  "endif

  "if empty(fileinfo)
    "return
  "endif

  let snippetsbarwinnr = bufwinnr('__SnippetsBar__')

  if &filetype == 'snippetsbar'
    let in_snippetsbar = 1
  else
    let in_snippetsbar = 0
    let prevwinnr = winnr()
    execute snippetsbarwinnr . 'wincmd w'
  endif

  "if !empty(s:known_files.getCurrent()) &&
        "\ fileinfo.fpath ==# s:known_files.getCurrent().fpath
    "" We're redisplaying the same file, so save the view
    "let saveline = line('.')
    "let savecol  = col('.')
    "let topline  = line('w0')
  "endif

  let lazyredraw_save = &lazyredraw
  set lazyredraw
  let eventignore_save = &eventignore
  set eventignore=all

  setlocal modifiable

  silent %delete _

  "let typeinfo = s:known_types[fileinfo.ftype]

  " Print tags
  "call s:PrintSnippets(fileinfo)
  call s:PrintSnippets(a:word)

  " Delete empty lines at the end of the buffer
  for linenr in range(line('$'), 1, -1)
    if getline(linenr) =~ '^$'
      execute linenr . 'delete _'
    else
      break
    endif
  endfor

  setlocal nomodifiable

  "if !empty(s:known_files.getCurrent()) &&
        "\ fileinfo.fpath ==# s:known_files.getCurrent().fpath
    "let scrolloff_save = &scrolloff
    "set scrolloff=0

    "call cursor(topline, 1)
    "normal! zt
    "call cursor(saveline, savecol)

    "let &scrolloff = scrolloff_save
  "else
    " Make sure as much of the snippetsbar content as possible is shown in the
    " window by jumping to the top after drawing
    execute 1
    call winline()
  "endif

  let &lazyredraw  = lazyredraw_save
  let &eventignore = eventignore_save

  if !in_snippetsbar
    execute prevwinnr . 'wincmd w'
  endif
endfunction

" s:RenderKeepView() {{{2
" The gist of this function was taken from NERDTree by Martin Grenfell.
function! s:RenderKeepView(...)
    if a:0 == 1
        let line = a:1
    else
        let line = line('.')
    endif

    let curcol  = col('.')
    let topline = line('w0')

    call s:RenderContent()

    let scrolloff_save = &scrolloff
    set scrolloff=0

    call cursor(topline, 1)
    normal! zt
    call cursor(line, curcol)

    let &scrolloff = scrolloff_save

    redraw
endfunction

" s:PrintSnippets {{{2
function! s:PrintSnippets(word)
  silent put =s:snippets
endfunction

"
" User Actions {{{1

" Helper Functions {{{1

" s:ultisnips_helper() {{{2
function! s:ultisnips_helper(word)
  exec "py vim.command('let snippets = \\'' + str(UltiSnips_Manager._snips('".a:word."', True)) + '\\'')"
  let snippets = substitute(snippets, 'Snippet(\([^,]\+\)\%("\%(\\.\|[^\\"]\)*"\|[^"]\)\{-})\(,\s*\)\?','\1\2', 'g')
  return sort(split(snippets[1:-2],',\s*'))
endfunction

" s:xptemplate_helper() {{{2
function! s:xptemplate_helper(word)
  if !exists('b:did_xpt')
    call XPTparseSnippets()
    let b:did_xpt = 1
  endif
  return sort(filter(keys(b:xptemplateData.filetypes[&filetype].allTemplates), 'v:val !~ ''\W\|^_'' && v:val =~? "^".a:word'))
endfunction

" s:set_engine() {{{2
function! s:set_engine()
  if exists('g:snippetsbar_engine')
    let s:engine = s:engines[tolower(g:snippetsbar)]
  else
    for key in keys(s:engines)
      if eval(s:engines[key].detect)
        let s:engine = s:engines[key]
        let s:engine.name = key
        break
      endif
    endfor
  endif
  if !exists('s:engine')
    "TODO: What to do here?
    echohl ErrorMsg
    echom 'SnippetsBar: No snippet engine was found!'
    echohl None
  else
    if !has_key(s:engine, 'pattern')
      let s:engine.pattern = '\W\zs\w\+\%#'
    endif
    if !has_key(s:engine, 'list_func_args')
      let s:engine.list_func_args = ''
    endif
  endif
endfunction

" s:CleanUp() {{{2
function! s:CleanUp()
    silent autocmd! SnippetsBarAutoCmds

    unlet s:is_maximized
    unlet s:compare_typeinfo
endfunction

" s:QuitIfOnlyWindow() {{{2
function! s:QuitIfOnlyWindow()
    " Before quitting Vim, delete the snippetsbar buffer so that
    " the '0 mark is correctly set to the previous buffer.
    if winbufnr(2) == -1
        " Check if there is more than one tab page
        if tabpagenr('$') == 1
            bdelete
            quit
        else
            close
        endif
    endif
endfunction

" s:AutoUpdate() {{{2
function! s:AutoUpdate()
  let p = searchpos(s:engine.pattern, 'bnW')
  let word = strpart(getline('.'), p[1]-1, col('.')-p[1])
  call s:Snippets(word)
    " Don't do anything if snippetsbar is not open or if we're in the snippetsbar window
    let snippetsbarwinnr = bufwinnr('__SnippetsBar__')
    if snippetsbarwinnr == -1 || &filetype == 'snippetsbar'
      return
    endif

    "" If we don't have an entry for the file by now something must have gone
    "" wrong, so don't change the snippetsbar content
    "if empty(fileinfo)
        "return
    "endif

    " Display the snippetsbar content
    "call s:RenderContent(fileinfo)
    call s:RenderContent(word)

    "" Call setCurrent after rendering so RenderContent can check whether the
    "" same file is redisplayed
    "if !empty(fileinfo)
        "call s:known_files.setCurrent(fileinfo)
    "endif

    "call s:HighlightTag()
endfunction

" s:Snippets(word) {{{2
function! s:Snippets(word)
  let args = eval('['.s:engine.list_func_args.']')
  let s:snippets = call(s:engine.list_func, args)
  if has_key(s:engine, 'list_map')
    call map(s:snippets, s:engine.list_map)
    echom 'map'
  endif
endfunction

" Commands {{{1
command! -nargs=0 SnippetsBarToggle        call s:ToggleWindow()
command! -nargs=0 SnippetsBarOpen          call s:OpenWindow(0)
command! -nargs=0 SnippetsBarOpenAutoClose call s:OpenWindow(1)
command! -nargs=0 SnippetsBarClose         call s:CloseWindow()

" Modeline {{{1
" vim: ts=8 sw=2 sts=2 et foldenable foldmethod=marker foldcolumn=1
