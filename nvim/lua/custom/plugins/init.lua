

return {
	{
		"aserowy/tmux.nvim",
		config = function() return require("tmux").setup() end
	},
	{
		"nvim-tree/nvim-tree.lua",
		requires = {
			"nvim-tree/nvim-web-devicons",
		},
--		config = function() return require("nvim-tree").setup() end
	},
	{
		"nvim-tree/nvim-web-devicons",
	},
	{
		"akinsho/bufferline.nvim",
		--version = "*",
		dependencies = 'nvim-tree/nvim-web-devicons',
--		config = function() return require("bufferline").setup() end
	},

}
