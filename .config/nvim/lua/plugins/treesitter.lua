-- No setup() on the main branch: install() is the whole API. It's async and a
-- no-op for parsers already present. Parsers and queries land in
-- stdpath("data")/site/{parser,queries}, which is already on runtimepath.
--
-- Only c/diff/lua/markdown/markdown_inline/query/vim/vimdoc ship with the nvim
-- binary, so without this every fenced code block in another language renders
-- as flat uncolored text.
require("nvim-treesitter").install({
    "bash",
    "css",
    "dockerfile",
    "fish",
    "go",
    "html",
    "javascript",
    "json",
    "nix",
    "python",
    "rust",
    "sql",
    "toml",
    "typescript",
    "yaml",
})
