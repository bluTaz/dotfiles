" ===========================================================
" QUICK-FIND
" ===========================================================
" [01] - PLUGIN MANAGER
" [02] - PLUGIN SETTINGS
" [03] - GENERAL UI CONFIGURATION
" [04] - GENERAL KEYBINDINGS
" ===========================================================
" Autodownload vim-plug
" ===========================================================
if empty(glob('~/.local/share/nvim/site/autoload/plug.vim'))
    silent !curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs
        \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
else
    autocmd VimEnter * source $MYVIMRC
endif

" ===========================================================
" [01] - PLUGIN MANAGER
" ===========================================================

call plug#begin('~/.local/share/nvim/plugged')

Plug 'tomtom/tcomment_vim'
Plug 'Yggdroot/indentLine'
Plug 'jiangmiao/auto-pairs'
Plug 'junegunn/vim-easy-align'
Plug 'SirVer/ultisnips' | Plug 'honza/vim-snippets'
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }
Plug 'chriskempson/base16-vim'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'airblade/vim-gitgutter'
Plug 'kien/ctrlp.vim'
Plug 'ervandew/supertab'

Plug 'mattn/emmet-vim', { 'for': 'html,css' }
" -----------------------------------------------------------
" deoplete sources
" -----------------------------------------------------------
Plug 'Shougo/neco-vim', { 'for': 'vim' } 		" vimscript
Plug 'zchee/deoplete-zsh', { 'for': 'zsh' } 	" zsh
Plug 'Shougo/context_filetype.vim'

" -----------------------------------------------------------
" Autocompletion with deoplete
" -----------------------------------------------------------
if has('nvim')
    Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
else
    Plug 'Shougo/deoplete.nvim'
    Plug 'roxma/nvim-yarp'
    Plug 'roxma/vim-hug-neovim-rpc'
endif


call plug#end()

" -----------------------------------------------------------
" Reload Config and Install Packages
" -----------------------------------------------------------
nmap <silent> <leader>fer :source $MYVIMRC <bar> :PlugClean <bar> :PlugInstall<cr>
nmap <silent> <leader>fed :edit $MYVIMRC <bar> :source $MYVIMRC<cr>

" ===========================================================
" [02] - PLUGIN SETTINGS
" ===========================================================
if filereadable(expand("~/.vimrc_background"))
    let base16colorspace=256
    source ~/.vimrc_background
endif

" -----------------------------------------------------------
"  Environment Settings [deoplete]
" -----------------------------------------------------------
let g:python_host_prog = '/usr/bin/python'
let g:python3_host_prog = '/usr/bin/python3'

" -----------------------------------------------------------
"  deoplete
" -----------------------------------------------------------
let g:UltiSnipsExpandTrigger="<C-j>"
inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
let g:deoplete#enable_at_startup = 1
let g:deoplete#enable_smart_case = 1
let g:deoplete#auto_refresh_delay = 0
let g:deoplete#auto_complete_delay = 0

" -----------------------------------------------------------
" Airline Theme
" -----------------------------------------------------------
let g:airline_theme = 'base16_twilight'
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#formatter = 'unique_tail_improved'
set noshowmode

" -----------------------------------------------------------
" CtrlP - fuzzy finder
" -----------------------------------------------------------
nmap <silent> <leader>ff :CtrlP<cr> 				" Find file
nmap <silent> <leader>fb :CtrlPBuffer<cr> 			" Find buffer
nmap <silent> <leader>fr :CtrlPMRUFiles<cr> 		" Find MRU

" -----------------------------------------------------------
"  indentLine
" -----------------------------------------------------------
" let g:indentLine_setColors = 0
let g:indentLine_char = '│'
let g:indentLine_first_char = '│'
let g:indentLine_showFirstIndentLevel = 1
" let g:indentLine_conceallevel = 1

" ===========================================================
" [03] - GENERAL UI CONFIGURATION
" ===========================================================
set number relativenumber
set cursorline
set termguicolors
set splitbelow
set splitright

" -----------------------------------------------------------
"  Tabs
" -----------------------------------------------------------
" set noexpandtab
set expandtab
set softtabstop =0
set shiftwidth =4
set tabstop =4
set smarttab
set cindent
set cinoptions=(0,u0,U0

" ===========================================================
" [04] - GENERAL KEYBINDINGS
" ===========================================================
let mapleader = "\<space>"

" -----------------------------------------------------------
" Window Navigation - Ctrl+{j,k,l,h}
" -----------------------------------------------------------
nnoremap <c-j> <c-w><c-j>
nnoremap <c-k> <c-w><c-k>
nnoremap <c-l> <c-w><c-l>
nnoremap <c-h> <c-w><c-h>

" -----------------------------------------------------------
" Buffer Navigation - Arrow Keys
" -----------------------------------------------------------
nnoremap <right> :bnext<cr>
nnoremap <left> :bprev<cr>

" -----------------------------------------------------------
"  Compatibility with ConEmu
" -----------------------------------------------------------
if !has("gui_running")
    set mouse=a
endif

set clipboard=unnamedplus

inoremap <Char-0x07F> <BS>
nnoremap <Char-0x07F> <BS>

" ===========================================================
" [05] - MY FUNCTIONS
" ===========================================================
" Return indent (all whitespace at start of a line), converted from
" tabs to spaces if what = 1, or from spaces to tabs otherwise.
" When converting to tabs, result has no redundant spaces.
function! Indenting(indent, what, cols)
    let spccol = repeat(' ', a:cols)
    let result = substitute(a:indent, spccol, '\t', 'g')
    let result = substitute(result, ' \+\ze\t', '', 'g')
    if a:what == 1
        let result = substitute(result, '\t', spccol, 'g')
    endif
    return result
endfunction

" Convert whitespace used for indenting (before first non-whitespace).
" what = 0 (convert spaces to tabs), or 1 (convert tabs to spaces).
" cols = string with number of columns per tab, or empty to use 'tabstop'.
" The cursor position is restored, but the cursor will be in a different
" column when the number of characters in the indent of the line is changed.
function! IndentConvert(line1, line2, what, cols)
    let savepos = getpos('.')
    let cols = empty(a:cols) ? &tabstop : a:cols
    execute a:line1 . ',' . a:line2 . 's/^\s\+/\=Indenting(submatch(0), a:what, cols)/e'
    call histdel('search', -1)
    call setpos('.', savepos)
endfunction
command! -nargs=? -range=% Space2Tab call IndentConvert(<line1>,<line2>,0,<q-args>)
command! -nargs=? -range=% Tab2Space call IndentConvert(<line1>,<line2>,1,<q-args>)
command! -nargs=? -range=% RetabIndent call IndentConvert(<line1>,<line2>,&et,<q-args>)


