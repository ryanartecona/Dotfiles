-- install lazy.nvim if not present
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
    vim.fn.system({
        "git",
        "clone",
        "--filter=blob:none",
        "https://github.com/folke/lazy.nvim.git",
        "--branch=stable", -- latest stable release
        lazypath,
    })
end
vim.opt.rtp:prepend(lazypath)

-- set leader before loading lazy
vim.g.mapleader = " "

-- split line to left of cursor, inverse of 'J'
vim.keymap.set('n', 'K', 'i<CR><Esc>k$', {remap = false})

-- lazy.nvim plugins
-- use `cond = not vim.g.vscode` as needed
local lazy = require('lazy')
lazy.setup({
  {
    -- https://github.com/kylechui/nvim-surround
    "kylechui/nvim-surround",
    version = "^3.0.0", -- Use for stability; omit to use `main` branch for the latest features
    event = "VeryLazy",
    config = function()
        require("nvim-surround").setup({
            -- Configuration here, or leave empty to use defaults
        })
    end
  },
  { 
    -- https://github.com/terryma/vim-expand-region
    'terryma/vim-expand-region',
    keys = {
      { "<Leader>v", "<Plug>(expand_region_expand)", mode = {"n", "v"} },
      { "<Leader>V", "<Plug>(expand_region_shrink)", mode = {"n", "v"} },
    }
  },
  {
    -- nvim-treesitter/nvim-treesitter-textobjects
    "nvim-treesitter/nvim-treesitter-textobjects",
    dependencies = {
      { "nvim-treesitter/nvim-treesitter", branch = "master", lazy = false, build = ":TSUpdate" },
    },
    lazy = false,
    config = function()
      require('nvim-treesitter.configs').setup({
        -- https://github.com/nvim-treesitter/nvim-treesitter?tab=readme-ov-file#incremental-selection
        -- NOTE: this only works while we use the nvim-treesitter master branch,
        -- but will eventually need reconfiguring upon switching to main. See
        -- https://pawelgrzybek.com/nvim-incremental-selection/.
        incremental_selection = {
          enable = true,
          keymaps = {
            -- set to `false` to disable one of the mappings
            init_selection = "<leader>n", 
            node_incremental = "<leader>n",
            scope_incremental = false,
            node_decremental = "<leader>N",
          },
        },
      })
      -- https://github.com/nvim-treesitter/nvim-treesitter-textobjects?tab=readme-ov-file#text-objects-swap
      -- swap next/previous elements of various parameter lists
      vim.keymap.set({"n", "x"}, "<leader>el", function()
        require("nvim-treesitter-textobjects.swap").swap_next "@parameter.inner"
      end)
      vim.keymap.set({"n", "x"}, "<leader>eh", function()
        require("nvim-treesitter-textobjects.swap").swap_previous "@parameter.inner"
      end)
    end,
  },
  { 
    -- https://github.com/echasnovski/mini.nvim/blob/main/readmes/mini-ai.md
    'echasnovski/mini.ai', 
    version = '*',
    config = function()
      local spec_treesitter = require('mini.ai').gen_spec.treesitter
      require('mini.ai').setup({
        custom_textobjects = {
          f = spec_treesitter({ a = '@function.outer', i = '@function.inner' }),
          o = spec_treesitter({
            a = { '@conditional.outer', '@loop.outer' },
            i = { '@conditional.inner', '@loop.inner' },
          }),
          e = spec_treesitter({
            a = '@parameter.outer',
            i = '@parameter.inner',
          }),
        }
      })
    end
  },
  -- {
  --   -- https://github.com/drybalka/tree-climber.nvim
  --   "drybalka/tree-climber.nvim",
  --   lazy = false,
  --   config = function()
  --     local tree_climber = require('tree-climber')
  --     local keyopts = { noremap = true, silent = false }
  --     local highlight = { highlight = true }
  --     local function highlighted(op)
  --       local function nested()
  --         return tree_climber[op](highlight)
  --       end
  --       return nested
  --     end
  --     vim.keymap.set({'n', 'x', 'o'}, 'gk', highlighted('goto_parent'), keyopts)
  --     vim.keymap.set({'n', 'x', 'o'}, 'gj', highlighted('goto_child'), keyopts)
  --     vim.keymap.set({'n', 'x', 'o'}, 'gl', highlighted('goto_next'), keyopts)
  --     vim.keymap.set({'n', 'x', 'o'}, 'gh', highlighted('goto_prev'), keyopts)
  --     vim.keymap.set({'x', 'o'}, 'in', tree_climber.select_node, keyopts)
  --     vim.keymap.set('n', '<leader>sl', highlighted('swap_next'), keyopts)
  --     vim.keymap.set('n', '<leader>sh', highlighted('swap_prev'), keyopts)
  --     vim.keymap.set('n', '<leader>h', tree_climber.highlight_node, keyopts)
  --     vim.keymap.set('n', '<c-i>', tree_climber.highlight_node, keyopts)
  --   end
  -- },
  {
    -- https://github.com/Dkendal/nvim-treeclimber
    "Dkendal/nvim-treeclimber",
    config = function()
      require('nvim-treeclimber').setup({})
    end,
    keys = {
      -- move selection around nodes
      {"gk", "<Plug>(treeclimber-select-parent)", mode = {"n", "x", "o"}, desc = "Select parent node"},
      {"gj", "<Plug>(treeclimber-select-shrink)", mode = {"n", "x", "o"}, desc = "Select child node"},
      {"gh", "<Plug>(treeclimber-select-previous)", mode = {"n", "x", "o"}, desc = "Select previous node"},
      {"gl", "<Plug>(treeclimber-select-next)", mode = {"n", "x", "o"}, desc = "Select next node"},
      -- select this node
      { "in", "<Plug>(treeclimber-select-current-node)", mode = { "x", "o" }, desc = "Select current node (inner)" },
      { "an", "<Plug>(treeclimber-select-expand)", mode = { "x", "o" }, desc = "Select parent node (around)" },
      -- Growth selection
      { "gp", "<Plug>(treeclimber-select-grow-backward)", mode = { "n", "x", "o" }, desc = "Grow selection backward" },
      { "gn", "<Plug>(treeclimber-select-grow-forward)", mode = { "n", "x", "o" }, desc = "Grow selection forward" },
      -- Sibling navigation
      { "g[", "<Plug>(treeclimber-select-siblings-backward)", mode = { "n", "x", "o" }, desc = "Select first sibling" },
      { "g]", "<Plug>(treeclimber-select-siblings-forward)", mode = { "n", "x", "o" }, desc = "Select last sibling" },
      -- Movement selection
      { "gb", "<Plug>(treeclimber-select-backward)", mode = { "n", "x", "o" }, desc = "Select and move to node start" },
      { "ge", "<Plug>(treeclimber-select-forward-end)", mode = { "n", "x", "o" }, desc = "Select and move to node end" },
    },
  }
}, {
  -- Lazy.nvim settings
  defaults = {
    reloadable = true, -- Makes plugins reloadable by default
  },
  change_detection = {
    enabled = true,   -- Detects changes to your config files
    notify = true,    -- Shows a notification when a change is detected
  },
})
