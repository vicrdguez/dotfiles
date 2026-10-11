-- Markdown snippets
return {
	{ prefix = "<s", body = "```$1\n$2\n```\n\n$0", desc = "Source block" },
	{ prefix = ">lua", body = "```lua\n$1\n```\n\n$0", desc = "lua source block" },
	{ prefix = ">sh", body = "```bash\n$1\n```\n\n$0", desc = "bash source block" },
	{ prefix = ">ex", body = "```elixir\n$1\n```\n\n$0", desc = "bash source block" },
}
