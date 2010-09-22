function! s:FindOtherBuffer(thisnr)
	let lastbuf = bufnr("$")
	let found = 0
	for a in range(1, lastbuf)
		if a:thisnr == a
			continue
		endif

		if bufexists(a)
			let found = a
		endif
	endfor

	return found
endfunction
	
function! s:Obliterate(expr)
	let thisnr = bufnr(a:expr)
	if (thisnr < 0)
		echo "Cannot find buffer for " . string(a:expr)
		return 1
	endif
	let diffnr = s:FindOtherBuffer(thisnr)
	let winnr = winnr()
	let a = 0
	if (!diffnr)
		execute "bd " . string(thisnr)
		return 0
	endif
	while 1 == 1
		let a = bufwinnr(thisnr)
		if a < 0
			break
		endif
		exe string(a) . "wincmd w"
		exe "buffer " . string(diffnr)
	endwhile
	exe "bd " . string(thisnr)
	exe string(winnr) . "wincmd w"
	return 0
endfunction

command! -nargs=1 -complete=buffer Obliterate call s:Obliterate(<f-args>)
