augroup project_settings
  autocmd!
  autocmd BufEnter * let b:start='ipython -i %'
  autocmd BufEnter * let b:dispatch='nosetests --with-doctest -v'
augroup END
