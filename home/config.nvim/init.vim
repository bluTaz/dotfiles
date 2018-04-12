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

Plug 'junegunn/vim-easy-align'
Plug 'SirVer/ultisnips' | Plug 'honza/vim-snippets'
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }
Plug 'chriskempson/base16-vim'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'airblade/vim-gitgutter'
Plug 'nathanaelkane/vim-indent-guides'
Plug 'kien/ctrlp.vim'
Plug 'ervandew/supertab'

" -----------------------------------------------------------
" deoplete sources
" -----------------------------------------------------------
Plug 'Shougo/neco-vim', { 'for': 'vim' } 		" vimscript
Plug 'zchee/deoplete-zsh', { 'for': 'zsh' } 	" zsh

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
nmap <silent> <leader>fR :source $MYVIMRC <bar> :PlugInstall<cr>

" ===========================================================
" [02] - PLUGIN SETTINGS
" ===========================================================
if filereadable(expand("~/.vimrc_background"))
	let base16colorspace=256
	source ~/.vimrc_background
endif

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
" vim-indent-guides
" -----------------------------------------------------------
let g:indent_guides_enable_on_vim_startup = 1

" -----------------------------------------------------------
"  deoplete
" -----------------------------------------------------------
let g:deoplete#enable_at_startup = 1
let g:UltiSnipsExpandTrigger="<C-j>"
inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"

" -----------------------------------------------------------
"  Environment Settings [deoplete]
" -----------------------------------------------------------
let g:python_host_prog = '/usr/bin/python'
let g:python3_host_prog = '/usr/bin/python3'

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
set noexpandtab
set softtabstop =0
set tabstop =4
set shiftwidth =4
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

