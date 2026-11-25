"""Build all paper figures with TikZ/PGFPlots and recorded benchmark data.

Requires Python (standard library), pdflatex, and pdftoppm. No benchmarks rerun.
All figures use 9 pt Latin Modern text at a common 160 mm export width.
"""
import csv
from decimal import Decimal, ROUND_HALF_UP
from pathlib import Path
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parent
OUT = ROOT / 'figures'
RESULTS = ROOT.parent / 'bench' / 'results'
PREAMBLE = r'''\documentclass[tikz,border=2pt]{standalone}
\usepackage{lmodern}
\usepackage{pgfplots}
\pgfplotsset{compat=1.18,every axis/.append style={
 font=\fontsize{9}{11}\selectfont,tick label style={font=\fontsize{9}{11}\selectfont},
 label style={font=\fontsize{9}{11}\selectfont},title style={font=\fontsize{9}{11}\selectfont},
 axis line style={black!50,line width=.4pt},tick style={black!50},
 axis lines*=left,clip=false,legend style={draw=none,font=\fontsize{9}{11}\selectfont},
 scaled ticks=false}}
\definecolor{accent}{HTML}{365F7D}
\begin{document}
\begin{tikzpicture}[font=\fontsize{9}{11}\selectfont]
'''
END = '\\end{tikzpicture}\n\\end{document}\n'

def read(name):
    with (RESULTS / name).open() as f:
        return list(csv.DictReader(f))

def coords(points):
    return ' '.join(f'({x},{y})' for x,y in points)

def compile_figure(name):
    with tempfile.TemporaryDirectory(prefix='couplr-figure-') as build:
        run = subprocess.run(['pdflatex', '-interaction=nonstopmode', '-halt-on-error',
                              f'-output-directory={build}', str(OUT / f'{name}.tex')],
                             capture_output=True, text=True)
        if run.returncode:
            raise RuntimeError(run.stdout[-6000:])
        (OUT / f'{name}.pdf').write_bytes((Path(build) / f'{name}.pdf').read_bytes())
    subprocess.run(['pdftoppm', '-singlefile', '-r', '180', '-png',
                    str(OUT / f'{name}.pdf'), str(OUT / name)], check=True)

memory = read('memory-results.csv')
s = PREAMBLE + r'''\path[use as bounding box] (0,0) rectangle (16,7.4);
\node[anchor=west] at (0,7.0) {\textbf{A}\quad Memory by sample size};
\node[anchor=west] at (8.1,7.0) {\textbf{B}\quad 20,000 observations};
\begin{axis}[at={(1.3cm,1.25cm)},anchor=south west,width=6.5cm,height=4.5cm,
 scale only axis,ymode=log,xmin=0,xmax=21,xtick={2,5,10,20},ytick={10,100,1000},
 yticklabels={10,100,1000},xlabel={Total observations (thousands)},
 ylabel={Peak memory above baseline (MB)},legend columns=3,
 legend style={at={(.5,1.12)},anchor=south,column sep=4pt}]
'''
ends=[]
for arm,color,mark in [('dense','black!55','square*'),('implicit','accent','*'),('lazy','black!30','triangle*')]:
    rows=sorted((r for r in memory if r['arm']==arm and r['status']=='ok'),key=lambda r:int(r['n_total']))
    s += r'\addplot[color='+color+',mark='+mark+r',mark size=1.7pt,line width=.7pt] coordinates {'+coords((int(r['n_total'])/1000,float(r['over_baseline_mb'])) for r in rows)+'};\n'
    s += r'\addlegendentry{'+arm.title()+'}\n'
    ends.append(next(r for r in rows if int(r['n_total'])==20000))
s += '\\end{axis}\n'
for x,field,title,limit,ticks in [(9.1,'over_baseline_mb','Memory (MB)',2400,'0,1500'),(13.1,'elapsed_s','Time (s)',10,'0,4,8')]:
    s += r'\begin{axis}[at={('+str(x)+r'cm,1.25cm)},anchor=south west,width=2.5cm,height=4.5cm,scale only axis,xbar,bar width=18pt,bar shift=0pt,xmin=0,xmax='+str(limit)+r',ymin=-.7,ymax=2.7,ytick={0,1,2},yticklabels={Lazy,Implicit,Dense},xtick={'+ticks+r'},title={'+title+r'},y axis line style={draw=none},tickwidth=0pt]'+'\n'
    for y,r,color in zip([2,1,0],ends,['black!55','accent','black!30']):
        v=float(r[field]); label=f'{v:,.1f}' if field=='over_baseline_mb' else str(Decimal(r[field]).quantize(Decimal('0.01'), rounding=ROUND_HALF_UP))
        s += r'\addplot[fill='+color+r',draw=none] coordinates {('+str(v)+','+str(y)+')};\n'
        s += r'\node[anchor=west,inner sep=2pt] at (axis cs:'+str(v)+','+str(y)+'){'+label+'};\n'
    s += '\\end{axis}\n'
(OUT/'memory-tradeoff.tex').write_text(s+END)

balance=sorted(read('lalonde-per-covariate.csv'),key=lambda r:float(r['unmatched']),reverse=True)
labels={'age':'Age','educ':'Education','race_Black':'Black','race_Hispanic':'Hispanic','married':'Married','nodegree':'No high-school degree','re74':'Earnings, 1974','re75':'Earnings, 1975'}
s=PREAMBLE+r'''\path[use as bounding box] (0,0) rectangle (16,7.4);
\node[anchor=west] at (0,7.0) {\textbf{A}\quad Covariate balance};
\node[anchor=west] at (11.4,7.0) {\textbf{B}\quad Sample composition};
\begin{axis}[at={(3.6cm,1.2cm)},anchor=south west,width=7.3cm,height=4.4cm,scale only axis,
 xmin=-.03,xmax=1.9,ymin=-.6,ymax=7.6,y dir=reverse,ytick={0,1,2,3,4,5,6,7},
 yticklabels={'''+','.join('{'+labels[r['covar']]+'}' for r in balance)+r'''},
 xtick={0,.5,1,1.5},xlabel={Absolute standardized mean difference},
 y axis line style={draw=none},ytick style={draw=none},legend columns=2,
 legend style={at={(.5,1.02)},anchor=south,column sep=8pt}]
\fill[black!4] (axis cs:0,-.6) rectangle (axis cs:.1,7.6);
\draw[dashed,black!45] (axis cs:.1,-.6) -- (axis cs:.1,7.6);
'''
for i,r in enumerate(balance):
    s+=r'\draw[black!25] (axis cs:'+r['unmatched']+','+str(i)+') -- (axis cs:'+r['couplr']+','+str(i)+');\n'
for field,style,label in [('unmatched','black!55,mark options={fill=white}','Before'),('couplr','accent','After')]:
    s+=r'\addplot[only marks,mark=*,mark size=2pt,'+style+'] coordinates {'+coords((r[field],i) for i,r in enumerate(balance))+'};\n'+r'\addlegendentry{'+label+'}\n'
s+=r'''\end{axis}
\begin{axis}[at={(12cm,1.2cm)},anchor=south west,width=3.5cm,height=4.4cm,scale only axis,
 ybar,bar width=27pt,bar shift=0pt,ymin=0,ymax=185,xmin=-.6,xmax=1.6,
 xtick={0,1},xticklabels={Treated,Controls},ytick={0,50,100,150},
 title={Black participants},tickwidth=2pt]
\addplot[fill=black!55,draw=none] coordinates {(0,156)};
\addplot[fill=black!55,draw=none] coordinates {(1,87)};
\node[anchor=south] at (axis cs:0,156) {156};
\node[anchor=south] at (axis cs:1,87) {87};
\end{axis}
'''
(OUT/'lalonde-balance.tex').write_text(s+END)
for name in ('matching-concept','memory-tradeoff','lalonde-balance'):
    compile_figure(name)
    print(f'Built {name}')


