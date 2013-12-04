module Marketing(canvasMarketing, footer) where

import JavaScript

manual1Text = """
Your goal is to keep the average driver happiness as high as possible.

The happiness goes up, when the car moves. The happiness goes down, if the car stands still.

If the car doesn’t move for a long time, the driver looses hope, and his happiness goes down to a level from which it’s hard to recover.
  """

manual2Text = """
You can either change the traffic lights manually, or set an automatic timer, which changes the left-right and top-down lights after specified intervals (in seconds).

Note that the actual change may take a while to come into effect.

You can also speed up and slow down the simulation.
"""

canvasMarketing canvas = 
  let (manualW, manualH) = (400.0, 200.0)
      manual1 = plainText manual1Text 
        |> size (round manualW) (round manualH) 
        |> toForm                                      
        |> move (-canvas.widthC/2+manualW/2+10, canvas.heightC/2-manualH/2)
      trafficHeader = toText "TRAFFIC" 
        |> header
        |> text
        |> container (round manualW) (round manualH) middle
        |> toForm                                      
        |> move (canvas.widthC/2-manualW/2-10, canvas.heightC/2-manualH/2)
      manual2 = plainText manual2Text 
        |> size (round manualW) (round manualH) 
        |> toForm                                      
        |> move (canvas.widthC/2-manualW/2-10, -canvas.heightC/2+manualH/2)
  in  group [ manual1, trafficHeader, manual2 ]

footerText = [markdown|
Built with [Elm](http://elm-lang.org/). 
Source code available @ [GitHub](https://github.com/adamw/traffic).

\(c\) 2013 [Adam Warski](http://warski.org/) and 
[SoftwareMill](https://softwaremill.com/)  
|]

codebrag = [markdown|
<a href="http://www.codebrag.com">
<img src="codebrag.jpg"  />
</a>
|]

footer = flow right [ footerText, spacer 47 10, codebrag ]