" markdownWikiLink is a new region
" syn region markdownWikiLink matchgroup=markdownLinkDelimiter start="\[\[" end="\]\]" contains=markdownUrl keepend oneline concealends
" " markdownLinkText is copied from runtime files with 'concealends' appended
" syn region markdownLinkText matchgroup=markdownLinkTextDelimiter start="!\=\[\%(\%(\_[^][]\|\[\_[^][]*\]\)*]\%( \=[[(]\)\)\@=" end="\]\%( \=[[(]\)\@=" nextgroup=markdownLink,markdownId skipwhite contains=@markdownInline,markdownLineStart concealends
" " markdownLink is copied from runtime files with 'conceal' appended
" syn region markdownLink matchgroup=markdownLinkDelimiter start="(" end=")" contains=markdownUrl keepend contained conceal
"
" syntax region markdownWikiAliasedLink start="\[\[[^\]]\+|" end="\]\]" keepend oneline contains=markdownLinkAlias,markdownLinkBody
" syntax region markdownLinkAlias start="|"ms=s+1 end=".+\]\]"me=e-2 keepend contained
" syntax region markdownLinkBody start="\[\["ms=s+2 end="|" keepend contained conceal

" syntax region tkAliasedLink start="\[\[[^\]]\+|" end="\]\]" keepend oneline contains=tkLinkAlias,tkLinkBody
" syntax region tkHighlightedAliasedLink start="\[\[[^\]]\+|" end="\]\]" keepend oneline contained contains=tkLinkAlias,tkLinkBody
" syntax region tkLinkAlias start="|"ms=s+1 end=".+\]\]"me=e-2 keepend contained
" syntax region tkLinkBody start="\[\["ms=s+2 end="|" keepend contained conceal
