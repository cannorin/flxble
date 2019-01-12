flxble - Fast Lightweight eXtensible BLog Engine
================================================

**\[WIP\]** flxble is a static site generator best suited for blogging, just like Jelyll or Hugo.

* **flxble is fast** .. It can process 1,000 markdown articles with less than 2 seconds.

* **flxble is lightweight** .. A standalone binary is available for Windows, Linux, and OS X, and takes just about 80MB.

* Not really extensible at the moment, but adding your own custom syntax to Markdown will be supported soon.

# Getting Started

If you have [.NET Core SDK](https://dotnet.microsoft.com/download) installed, you can easily install flxble by `dotnet tool install -g flxble`.

Alternatvely, you can just download a standalone binary from [Releases](https://github.com/cannorin/flxble/releases). It contains the executable `flxble[.exe]` along with all the dependencies, so you can put the unpacked directory anywhere you want.

# How to use

First, execute `flxble init-blog` and the empty blog will be created in the current directory.

`flxble new-page src/blog/<any_name>.md` to create a new page.

Finally, `flxble render-blog` to turn the articles into a blog.

# Config file - `flxble.toml`

```toml
# Required
base_url = 'http://example.com'
title = "my blog"
theme = "casper" # to disable theming, specify "none".

# Optional, will be empty by default
subtitle = ""
description = '''
enter some description here
'''
copyright = "Copyright 2018 <your_name>"

# Optional, will be this value by default
lang    = "en-us"
source  = "src"     # directory to find source files
output  = "output"  # directory to output processed files
content = "content" # files in this directory will be copied directly to ./${output}/
themes  = "themes"  # directory to find themes

# Optional, disabled by default
tag     = "tag"     # tag archives will be generated to
                    # ./${output}/${tag}/${tag-name}.html
archive = "archive" # monthly archives will be generated to
                    # ./${output}/${archive}/${yyyy}-${mm}.html
# cache   = ".cache"  # directory to store caches for skipping unchanged documents
                      # (not implemented yet)

## Markdown config (optional) 
markdown = "common"   # set this "advanced" to enable popular extensions
# markdown_extra_rules = "extra_rules"
                      # put your custom rule definitions here (not implemented yet)
```

# Theming

flxble supports decent theming. See the [minimal template](https://github.com/cannorin/flxble/tree/master/src/blog_template/themes/minimal) for example.

# Documentation

TBD

# License

Apache 2.0.
