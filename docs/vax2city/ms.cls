\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{ms}[2021]
% article inheritance
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}}
\ProcessOptions
\LoadClass{article}
% packages
\RequirePackage[margin=4cm]{geometry}
\RequirePackage{authblk}
% references
\usepackage[
  style=numeric-comp,
  doi=false,
  url=true,
  natbib=true,
  backend=biber,
  sorting=none,
]{biblatex}
\renewcommand{\bibfont}{\footnotesize}
\DeclareSourcemap{\maps[datatype=bibtex]{
  \map{\step[typesource=techreport, typetarget=report]}}}
\renewcommand*{\multicitedelim}{\addcomma}
% spacing
\frenchspacing
\setlength{\parskip}{1ex}
\setlength{\parindent}{0ex}
\setlength{\abovecaptionskip}{\medskipamount}
\setlength{\belowcaptionskip}{\medskipamount}
\widowpenalty10000
\clubpenalty10000
\renewcommand{\floatpagefraction}{0.9}
% meta-data
\renewcommand{\title}[2][]{\gdef\@runtitle{#1}\gdef\@title{#2}}
\newcommand{\journal}[1]{\gdef\@journal{#1}}
% \newcommand{\keywords}[1]{\gdef\@keywords{#1}}
\renewcommand{\abstract}[1]{\gdef\@abstract{#1}}
% \newcommand{\highlights}[1]{\gdef\@highlights{#1}}
\newcommand{\data}[1]{\gdef\@data{#1}}
\newcommand{\funding}[1]{\gdef\@funding{#1}}
\newcommand{\acknowl}[1]{\gdef\@acknowl{#1}}
\newcommand{\contrib}[1]{\gdef\@contrib{#1}}
% helpers
\renewcommand{\paragraph}[1]{\par\textbf{#1:}}
\newcommand{\headstyle}[1]{\normalfont\MakeUppercase{#1}}
\newcommand{\tstyle}[1]{\textbf{#1}}
\newcommand{\tpart}[3][:]{%
  \hspace{0.14\linewidth}\llap{\tstyle{#2#1}}\hspace{0.01\linewidth}%
  \noindent\parbox[t]{0.85\textwidth}{#3}\par\bigskip}
% maketitle
\renewcommand{\maketitle}{%
  \thispagestyle{empty}
  \tpart{Title}{\@title}
  \tpart{Authors}{\@author}
  \tpart{Journal}{\@journal}
  \tpart{Date}{\@date}
  \clearpage
  {\tstyle{Abstract}\par\bigskip\@abstract\par}
  \clearpage\pagestyle{plain}}
\newcommand{\backmatter}{%
  \clearpage
  \subsection*{Funding}\@funding
  \subsection*{Acknowledgements}\@acknowl
  \subsection*{Contributions}\@contrib
  \subsection*{Data Availability}\@data
  \clearpage}
% appendix
\let\xappendix\appendix
\renewcommand{\appendix}{
  \xappendix
  \clearpage
  \setcounter{page}{1}
  \renewcommand*{\thepage}{A.\arabic{page}}
  \counterwithin{table}{section}
  \counterwithin{figure}{section}
  \counterwithin{equation}{section}
  \thispagestyle{plain}
  \hspace{0.15\linewidth}{\large\textsc{appendix}}
  \vskip1em
  \tpart{Title}{\@title}
  \tpart{Authors}{\@author}
  \tpart{Date}{\@date}
  \refsection
  \clearpage}
