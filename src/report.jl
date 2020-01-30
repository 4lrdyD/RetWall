#revisión 0.0.1 30-01-2020, 01:00 Julia1.1.0
function report(hp,hz,t1,t2,t3,b1,b2,grav)
a="
\\documentclass[oneside,spanish]{scrbook}
\\usepackage[spanish, es-nodecimaldot, es-tabla]{babel}
\\usepackage{amsmath}
\\usepackage{tikz}
\\usepackage{pgfplots}
\\pgfplotsset{compat=newest}
\\usepgfplotslibrary{units}
\\begin{document}
\\chapter{Geometría del muro}
Las dimensiones del muro son:\\\\
\\begin{align*}
    hp &= $hp m\\\\
    hz &= $hz m\\\\
    t1 &= $t1 m\\\\
    t2 &= $t2 m\\\\
    t3 &= $t3 m\\\\
    b1 &= $b1 m\\\\
    b2 &= $b2 m\\\\
\\end{align*}
\\begin{figure}
	\\centering
    \\begin{tikzpicture}

      \\begin{axis}[
          width=\\linewidth, yticklabels=\\empty,xticklabels=\\empty,
            ytick=\\empty,xtick=\\empty,axis line style={draw=none}
        ]
        \\addplot[black=50!]
        table[x=x,y=y,col sep=&,row sep=\\\\] {x & y\\\\
		$(grav.nod[grav.elm[1,1],1]) & $(grav.nod[grav.elm[1,1],2])\\\\
		$(grav.nod[grav.elm[1,2],1]) & $(grav.nod[grav.elm[1,2],2])\\\\
		$(grav.nod[grav.elm[1,3],1]) & $(grav.nod[grav.elm[1,3],2])\\\\
		$(grav.nod[grav.elm[1,1],1]) & $(grav.nod[grav.elm[1,1],2])\\\\};
        \\addplot[black=50!]
        table[x=x,y=y,col sep=&,row sep=\\\\] {x & y\\\\
		$(grav.nod[grav.elm[2,1],1]) & $(grav.nod[grav.elm[2,1],2])\\\\
		$(grav.nod[grav.elm[2,2],1]) & $(grav.nod[grav.elm[2,2],2])\\\\
		$(grav.nod[grav.elm[2,3],1]) & $(grav.nod[grav.elm[2,3],2])\\\\
		$(grav.nod[grav.elm[2,1],1]) & $(grav.nod[grav.elm[2,1],2])\\\\};
        \\addplot[black=50!]
        table[x=x,y=y,col sep=&,row sep=\\\\] {x & y\\\\
		$(grav.nod[grav.elm[3,1],1]) & $(grav.nod[grav.elm[3,1],2])\\\\
		$(grav.nod[grav.elm[3,2],1]) & $(grav.nod[grav.elm[3,2],2])\\\\
		$(grav.nod[grav.elm[3,3],1]) & $(grav.nod[grav.elm[3,3],2])\\\\
		$(grav.nod[grav.elm[3,4],1]) & $(grav.nod[grav.elm[3,4],2])\\\\
		$(grav.nod[grav.elm[3,1],1]) & $(grav.nod[grav.elm[3,1],2])\\\\};
        \\addplot[black=50!]
        table[x=x,y=y,col sep=&,row sep=\\\\] {x & y\\\\
		$(grav.nod[grav.elm[4,1],1]) & $(grav.nod[grav.elm[4,1],2])\\\\
		$(grav.nod[grav.elm[4,2],1]) & $(grav.nod[grav.elm[4,2],2])\\\\
		$(grav.nod[grav.elm[4,3],1]) & $(grav.nod[grav.elm[4,3],2])\\\\
		$(grav.nod[grav.elm[4,4],1]) & $(grav.nod[grav.elm[4,4],2])\\\\
		$(grav.nod[grav.elm[4,1],1]) & $(grav.nod[grav.elm[4,1],2])\\\\};
        \\draw ($(grav.prop[1,2]),$(grav.prop[1,3]))node{\\small{1}};
        \\draw ($(grav.prop[2,2]),$(grav.prop[2,3]))node{\\small{2}};
        \\draw ($(grav.prop[3,2]),$(grav.prop[3,3]))node{\\small{3}};
        \\draw ($(grav.prop[4,2]),$(grav.prop[4,3]))node{\\small{4}};
      \\end{axis}
    \\end{tikzpicture}
  \\caption{Muro de contención de gravedad}
	\\label{fig:spectre1}
\\end{figure}
\\end{document}
"
open("prueba1.tex", "w") do f
           write(f, a)
           end
#run(pipeline(`pdflatex prueba1`,stdout="log.txt",stderr="err.txt"));
run(`pdflatex prueba1`);
run(`cmd /c start prueba1.pdf`);
end
