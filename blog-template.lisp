(html
  (head
    (style
      (css
	"html" (background-color black)
	"body" (margin 0)
	"#navbar" (overflow hidden background-color "#333")
	"#navbar a" (float left color "#f2f2f2" text-align center
			   text-decoration none padding "10px 14px")
	"#navbar a:hover" (color black background-color "#ddd")
	"#main" (width "60%" padding "0 20%" )
	"#main div" (
		     width "100%" text-align center
		     color white float left
		     outline solid outline-color white
		     outline-width "2px"))))
  (body
    (div :attrs (id "navbar")
      (a :attrs (href "/")
	(b "home"))
      (a :attrs (href "/site-generator")
	(b "Site Generator")))
    (div :attrs (id "main")
      (div
	(p "hello world!")))))
