\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{hph}[2017/06/17 A class file for documents written in the Hratchian Group at UC Merced.]
\LoadClass[11pt]{report}
%
% This LaTeX class file provides a number of format options and tools used
% in the Hratchian Group at the University of California, Merced.
%
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Section 1. Load other packages.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%hph \usepackage{sfmath}
\RequirePackage{sfmath}
\RequirePackage{soul,varwidth}
\RequirePackage[normalem]{ulem}
\RequirePackage{ifthen,floatflt,graphicx,setspace,subfigure,xcolor}
\RequirePackage[version=3]{mhchem} % Formula subscripts using \ce{}
\RequirePackage[margin=1.0in]{geometry}
\RequirePackage{fancyhdr}
\RequirePackage{helvet}
\RequirePackage{layouts}
\RequirePackage{upgreek,amsmath,amsthm,amssymb,mathrsfs,xfrac,mathtools,dsfont}
\RequirePackage{parskip}
\RequirePackage[compact,medium]{titlesec}
\RequirePackage{enumitem,etoolbox}
\RequirePackage{lipsum}
\RequirePackage[biblabel=period,usetitle=true]{achemso}
\RequirePackage{needspace}
\RequirePackage{verbatim}

%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Section 2. Define and initialize package options.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
\newboolean{HPHHW@DueDateEndOfClass}
\setboolean{HPHHW@DueDateEndOfClass}{true}
\newboolean{HPHHW@DueDateMidnight}
\setboolean{HPHHW@DueDateMidnight}{false}
\DeclareOption{dueendofclass}{
  \setboolean{HPHHW@DueDateEndOfClass}{true}
  \setboolean{HPHHW@DueDateMidnight}{false}
}
\DeclareOption{duemidnight}{
  \setboolean{HPHHW@DueDateEndOfClass}{false}
  \setboolean{HPHHW@DueDateMidnight}{true}
}
\DeclareOption{noduedate}{
  \setboolean{HPHHW@DueDateEndOfClass}{false}
  \setboolean{HPHHW@DueDateMidnight}{false}
}
\DeclareOption{nodate}{
  \setboolean{HPHHW@DueDateEndOfClass}{false}
  \setboolean{HPHHW@DueDateMidnight}{false}
}
%
\newboolean{HPHHW@SolutionsVersion}
\setboolean{HPHHW@SolutionsVersion}{false}
\DeclareOption{solutions}{
  \setboolean{HPHHW@SolutionsVersion}{true}
}
%
\newboolean{HPHHW@SpaceForSolutions}
\setboolean{HPHHW@SpaceForSolutions}{false}
\DeclareOption{blanksolutions}{
  \setboolean{HPHHW@SpaceForSolutions}{true}
}
\DeclareOption{noblanksolutions}{
  \setboolean{HPHHW@SpaceForSolutions}{false}
}
%
\newboolean{HPHHW@ShowProblemValues}
\setboolean{HPHHW@ShowProblemValues}{false}
\DeclareOption{points}{
  \setboolean{HPHHW@ShowProblemValues}{true}
}
%
\newboolean{HPHHW@FinalExamAssignment}
\setboolean{HPHHW@FinalExamAssignment}{false}
\newboolean{HPHHW@MidtermExamAssignment}
\setboolean{HPHHW@MidtermExamAssignment}{false}
\newboolean{HPHHW@ExamAssignment}
\setboolean{HPHHW@ExamAssignment}{false}
\newboolean{HPHHW@QuizAssignment}
\setboolean{HPHHW@QuizAssignment}{false}
\newboolean{HPHHW@InClassExerciseSet}
\setboolean{HPHHW@InClassExerciseSet}{false}
\newboolean{HPHHW@HomeworkAssignment}
\setboolean{HPHHW@HomeworkAssignment}{false}
\newboolean{HPHHW@ProblemSet}
\setboolean{HPHHW@ProblemSet}{false}
\newboolean{HPHHW@GenericAssignment}
\setboolean{HPHHW@GenericAssignment}{false}
\newboolean{HPHHW@Notes}
\setboolean{HPHHW@Notes}{true}
\newcommand{\HPHHW@ActivityType}{ABC}
\newcommand{\HPHHW@ActivityTitle}{ABCDEFG}
\DeclareOption{finalexam}{
  \setboolean{HPHHW@FinalExamAssignment}{true}
  \setboolean{HPHHW@MidtermExamAssignment}{false}
  \setboolean{HPHHW@ExamAssignment}{false}
  \setboolean{HPHHW@QuizAssignment}{false}
  \setboolean{HPHHW@InClassExerciseSet}{false}
  \setboolean{HPHHW@HomeworkAssignment}{false}
  \setboolean{HPHHW@ProblemSet}{false}
  \setboolean{HPHHW@GenericAssignment}{false}
  \setboolean{HPHHW@Notes}{false}
  \renewcommand{\HPHHW@ActivityType}{Final Exam}
  \renewcommand{\HPHHW@ActivityTitle}{Final Exam}
}
\DeclareOption{midtermexam}{
  \setboolean{HPHHW@FinalExamAssignment}{false}
  \setboolean{HPHHW@MidtermExamAssignment}{true}
  \setboolean{HPHHW@ExamAssignment}{false}
  \setboolean{HPHHW@QuizAssignment}{false}
  \setboolean{HPHHW@InClassExerciseSet}{false}
  \setboolean{HPHHW@HomeworkAssignment}{false}
  \setboolean{HPHHW@ProblemSet}{false}
  \setboolean{HPHHW@GenericAssignment}{false}
  \setboolean{HPHHW@Notes}{false}
  \renewcommand{\HPHHW@ActivityType}{Midterm Exam}
  \renewcommand{\HPHHW@ActivityTitle}{Midterm Exam}
}
\DeclareOption{exam}{
  \setboolean{HPHHW@FinalExamAssignment}{false}
  \setboolean{HPHHW@MidtermExamAssignment}{false}
  \setboolean{HPHHW@ExamAssignment}{true}
  \setboolean{HPHHW@QuizAssignment}{false}
  \setboolean{HPHHW@InClassExerciseSet}{false}
  \setboolean{HPHHW@HomeworkAssignment}{false}
  \setboolean{HPHHW@ProblemSet}{false}
  \setboolean{HPHHW@GenericAssignment}{false}
  \setboolean{HPHHW@Notes}{false}
  \renewcommand{\HPHHW@ActivityType}{Exam}
  \renewcommand{\HPHHW@ActivityTitle}{Exam}
}
\DeclareOption{quiz}{
  \setboolean{HPHHW@FinalExamAssignment}{false}
  \setboolean{HPHHW@MidtermExamAssignment}{false}
  \setboolean{HPHHW@ExamAssignment}{false}
  \setboolean{HPHHW@QuizAssignment}{true}
  \setboolean{HPHHW@InClassExerciseSet}{false}
  \setboolean{HPHHW@HomeworkAssignment}{false}
  \setboolean{HPHHW@ProblemSet}{false}
  \setboolean{HPHHW@GenericAssignment}{false}
  \setboolean{HPHHW@Notes}{false}
  \renewcommand{\HPHHW@ActivityType}{Quiz}
  \renewcommand{\HPHHW@ActivityTitle}{Quiz}
}
\DeclareOption{inclassex}{
  \setboolean{HPHHW@FinalExamAssignment}{false}
  \setboolean{HPHHW@MidtermExamAssignment}{false}
  \setboolean{HPHHW@ExamAssignment}{false}
  \setboolean{HPHHW@QuizAssignment}{false}
  \setboolean{HPHHW@InClassExerciseSet}{true}
  \setboolean{HPHHW@HomeworkAssignment}{false}
  \setboolean{HPHHW@ProblemSet}{false}
  \setboolean{HPHHW@GenericAssignment}{false}
  \setboolean{HPHHW@Notes}{false}
  \renewcommand{\HPHHW@ActivityType}{In-Class Exercise Set}
  \renewcommand{\HPHHW@ActivityTitle}{In-Class Exercise Set}
}
\DeclareOption{homework}{
  \setboolean{HPHHW@FinalExamAssignment}{false}
  \setboolean{HPHHW@MidtermExamAssignment}{false}
  \setboolean{HPHHW@ExamAssignment}{false}
  \setboolean{HPHHW@QuizAssignment}{false}
  \setboolean{HPHHW@InClassExerciseSet}{false}
  \setboolean{HPHHW@HomeworkAssignment}{true}
  \setboolean{HPHHW@ProblemSet}{false}
  \setboolean{HPHHW@GenericAssignment}{false}
  \setboolean{HPHHW@Notes}{false}
  \renewcommand{\HPHHW@ActivityType}{Homework}
  \renewcommand{\HPHHW@ActivityTitle}{Homework Assignment}
}
\DeclareOption{problemset}{
  \setboolean{HPHHW@FinalExamAssignment}{false}
  \setboolean{HPHHW@MidtermExamAssignment}{false}
  \setboolean{HPHHW@ExamAssignment}{false}
  \setboolean{HPHHW@QuizAssignment}{false}
  \setboolean{HPHHW@InClassExerciseSet}{false}
  \setboolean{HPHHW@HomeworkAssignment}{false}
  \setboolean{HPHHW@ProblemSet}{true}
  \setboolean{HPHHW@GenericAssignment}{false}
  \setboolean{HPHHW@Notes}{false}
  \renewcommand{\HPHHW@ActivityType}{Problem Set}
  \renewcommand{\HPHHW@ActivityTitle}{Problem Set}
}
\DeclareOption{genericassignment}{
  \setboolean{HPHHW@FinalExamAssignment}{false}
  \setboolean{HPHHW@MidtermExamAssignment}{false}
  \setboolean{HPHHW@ExamAssignment}{false}
  \setboolean{HPHHW@QuizAssignment}{false}
  \setboolean{HPHHW@InClassExerciseSet}{false}
  \setboolean{HPHHW@HomeworkAssignment}{false}
  \setboolean{HPHHW@ProblemSet}{false}
  \setboolean{HPHHW@GenericAssignment}{true}
  \setboolean{HPHHW@Notes}{false}
  \renewcommand{\HPHHW@ActivityType}{Generic}
  \renewcommand{\HPHHW@ActivityTitle}{Generic}
}
\DeclareOption{notes}{
  \setboolean{HPHHW@FinalExamAssignment}{false}
  \setboolean{HPHHW@MidtermExamAssignment}{false}
  \setboolean{HPHHW@ExamAssignment}{false}
  \setboolean{HPHHW@QuizAssignment}{false}
  \setboolean{HPHHW@InClassExerciseSet}{false}
  \setboolean{HPHHW@HomeworkAssignment}{false}
  \setboolean{HPHHW@ProblemSet}{false}
  \setboolean{HPHHW@GenericAssignment}{false}
  \setboolean{HPHHW@Notes}{true}
  \renewcommand{\HPHHW@ActivityType}{Notes}
  \renewcommand{\HPHHW@ActivityTitle}{Notes}
}
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Section 3. Process package option settings and define/
%%             initialize a few macros and settings for
%%             defining general layout options.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
\ProcessOptions\relax
%
%
% Set the text and turn hyphenation off.
%
\renewcommand{\familydefault}{\sfdefault}
\RequirePackage[none]{hyphenat}
%
%
% Set margin widths.
%
\oddsidemargin 0.0 in
\evensidemargin 0.0 in
%
%
% Set-up macros and commands for header and title fields.
%
%   \setcoursename, \HPHHW@CourseName
\newcommand{\HPHHW@CourseName}{Unknown}
\newcommand{\setcoursename}[1]{
  \renewcommand{\HPHHW@CourseName}{#1}
}
%
%   \setduedate, \HPHHW@DueDate
\newcommand{\HPHHW@DueDate}{Unknown}
\newcommand{\setduedate}[1]{
  \renewcommand{\HPHHW@DueDate}{#1}
}
%
%   \setActivityNumber, \HPHHW@ActivityNumber
\newcommand{\HPHHW@ActivityNumber}{Unknown}
\newcommand{\setactivitynumber}[1]{
  \ifthenelse{\boolean{HPHHW@ExamAssignment}}
  {
    \renewcommand{\HPHHW@ActivityNumber}{\uppercase\expandafter{\romannumeral #1\relax}}
  }{
    \renewcommand{\HPHHW@ActivityNumber}{#1}
  }
}
%
%   \genericname, \HPHHW@GenericName
\newcommand{\HPHHW@GenericName}{Unknown}
\newcommand{\genericname}[1]{
  \renewcommand{\HPHHW@GenericName}{#1}
  \renewcommand{\HPHHW@ActivityTitle}{#1}
}
%
%   \setauthor, \HPHHW@Author
\newcommand{\HPHHW@Author}{Unknown}
\newcommand{\setauthor}[1]{
  \renewcommand{\HPHHW@Author}{#1}
}
%
%   \setrunningtitle, \HPHHW@RunningTitle
\newcommand{\HPHHW@RunningTitle}{Unknown}
\newcommand{\setrunningtitle}[1]{
  \renewcommand{\HPHHW@RunningTitle}{#1}
}
%
%   \setdate, \HPHHW@Date
\newcommand{\HPHHW@Date}{Unknown}
\newcommand{\setdate}[1]{
  \renewcommand{\HPHHW@Date}{#1}
}
%
%   \settitle
\newcommand{\settitle}[1]{
  \renewcommand{\HPHHW@ActivityTitle}{#1}
}
%
%   \settypee
\newcommand{\settype}[1]{
  \renewcommand{\HPHHW@ActivityType}{#1}
}
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Section 4. Set-up headers and footers. Note that the
%%             header and footer for the first page are
%%             different from those on subsequent pages.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
\newcommand{\makeheaderfooter}{
  \pagestyle{fancy}
  \ifthenelse{\boolean{HPHHW@Notes}}
  {
    \lhead{\footnotesize\textit{\HPHHW@RunningTitle}}
  }{
    \lhead{\footnotesize\textsc{\HPHHW@CourseName}}
  }
  \ifthenelse{\boolean{HPHHW@Notes}}
  {
    \rhead{\footnotesize{}\textit\HPHHW@Author}
  }{
    \ifthenelse{\boolean{HPHHW@FinalExamAssignment}\OR
    \boolean{HPHHW@MidtermExamAssignment}}
    {
      \rhead{\footnotesize{}\HPHHW@ActivityType{}}
    }{\ifthenelse{\boolean{HPHHW@GenericAssignment}}
      {
        \rhead{\footnotesize{}\HPHHW@GenericName}
      }{
        \rhead{\footnotesize{}\HPHHW@ActivityType{} \HPHHW@ActivityNumber}
      }
    }
  }
  \cfoot{\footnotesize{}\thepage}
  \renewcommand{\headrulewidth}{0.0pt}
  \renewcommand{\footrulewidth}{0.3pt}
}
\ifthenelse{\boolean{HPHHW@SolutionsVersion}}
{
  \fancypagestyle{firststyle}{
    \lhead{\textsc{\HPHHW@CourseName}}
    \rhead{\textsc{Solutions}}
    \cfoot{}
    \renewcommand{\headrulewidth}{0.0pt}
    \renewcommand{\footrulewidth}{0.0pt}
  }
}{
  \ifthenelse{\boolean{HPHHW@Notes}}
  { 
    \fancypagestyle{firststyle}{
      \lhead{\textsc{\HPHHW@Date}}
      \rhead{\HPHHW@Author}
      \cfoot{}
      \renewcommand{\headrulewidth}{0.0pt}
      \renewcommand{\footrulewidth}{0.0pt}
    }
  }{
    \fancypagestyle{firststyle}{
      \lhead{\textsc{\HPHHW@CourseName}}
      \rhead{Name: \underline{\hspace{2.5in}}}
      \cfoot{}
      \renewcommand{\headrulewidth}{0.0pt}
      \renewcommand{\footrulewidth}{0.0pt}
    }
  }
}
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Section 5. Maketitle settings.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
\renewcommand{\maketitle}{%
  \thispagestyle{firststyle}
  \begin{center}
    \ifthenelse{\boolean{HPHHW@ProblemSet}}
    {
      {\large{\textbf{\HPHHW@ActivityTitle{}}}}\\
      {\textbf{\HPHHW@ActivityNumber}}\\
    }{
      \ifthenelse{\boolean{HPHHW@FinalExamAssignment}\OR
      \boolean{HPHHW@MidtermExamAssignment}\OR
      \boolean{HPHHW@GenericAssignment}\OR
      \boolean{HPHHW@Notes}}
      {
        {\large{\textbf{\HPHHW@ActivityTitle{}}}}\\
      }{
        {\large{\textbf{\HPHHW@ActivityTitle{} \HPHHW@ActivityNumber}}}\\
      }
      \ifthenelse{\boolean{HPHHW@HomeworkAssignment}}
      {
        \vspace{3mm}
        \textit{Complete the following problems.}\\
        \ifthenelse{\boolean{HPHHW@DueDateEndOfClass}}
          {\textit{\textbf{This assignment is due at the start of class on \HPHHW@DueDate.}}\\}
          {}
        \ifthenelse{\boolean{HPHHW@DueDateMidnight}}
          {\textit{\textbf{This assignment is due by 11:59~pm on \HPHHW@DueDate.}}\\}
          {}
      }{
        \ifthenelse{\NOT\boolean{HPHHW@Notes}}
        {
          \textbf{\HPHHW@DueDate}\\
        }
      }
    }

    \vspace{-2mm}
    \hrulefill{}
  \end{center}
}
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Section 6. Settings specific to the 'problem' counters
%%             and printing.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
\newlength{\HPHHW@CurrentProblemWidth}
\newlength{\HPHHW@CurrentSubProblemWidth}
\newlength{\HPHHW@ProblemIndent}
\setlength{\HPHHW@ProblemIndent}{0.25 in}
\newcounter{HPHHW@ProblemNumberCounter}\setcounter{HPHHW@ProblemNumberCounter}{0}
\newcounter{HPHHW@ProblemValueSum}\setcounter{HPHHW@ProblemValueSum}{0}
\newcounter{HPHHW@SubProblemNumberCounter}\setcounter{HPHHW@SubProblemNumberCounter}{0}
%
\newlist{Problem1List}{enumerate}{1}
\setlist[Problem1List]{align=left,label=\arabic*.,labelwidth=5mm,leftmargin=!}
\newtoggle{FirstProblem1}
\toggletrue{FirstProblem1}
\newenvironment{problem1}
{
  \iftoggle{FirstProblem1}%
  {
    \begin{Problem1List}[series=numproblems]%
    \global\togglefalse{FirstProblem1}%
    \toggletrue{FirstSubProblem1}%
    \setcounter{HPHHW@SubProblemNumberCounter}{0}%
  }{
    \begin{Problem1List}[resume=numproblems]%
    \global\togglefalse{FirstProblem1}%
    \toggletrue{FirstSubProblem1}%
    \setcounter{HPHHW@SubProblemNumberCounter}{0}%
  }
  \item\ignorespaces%
}{
  \end{Problem1List}
  \vspace{8mm}
}
%
\newcommand{\problem}[1]{
  \vspace{3mm}
  \toggletrue{FirstSubProblem1}%
  \setcounter{HPHHW@SubProblemNumberCounter}{0}
  \setlength{\HPHHW@CurrentProblemWidth}{\linewidth}
  \addtolength{\HPHHW@CurrentProblemWidth}{-\HPHHW@ProblemIndent}
  \begin{minipage}[t]{\HPHHW@ProblemIndent}
    \addtocounter{HPHHW@ProblemNumberCounter}{1}%
    \theHPHHW@ProblemNumberCounter{}.%
  \end{minipage}
  \begin{minipage}[t]{\HPHHW@CurrentProblemWidth}
    #1%
  \end{minipage}
}
%
\newlist{SubProblem1List}{enumerate}{1}
\setlist[SubProblem1List]{align=left,label=\alph*.,labelwidth=3mm,leftmargin=!}
\newtoggle{FirstSubProblem1}
\toggletrue{FirstSubProblem1}
\newenvironment{subproblem1}
{
  \vspace{2mm}
  \iftoggle{FirstSubProblem1}%
  {
    \begin{SubProblem1List}[series=numsubproblems]%
    \global\togglefalse{FirstSubProblem1}%
  }{
    \begin{SubProblem1List}[resume=numsubproblems]%
    \global\togglefalse{FirstSubProblem1}%
  }
  \item\ignorespaces%
}{
  \end{SubProblem1List}
  \vspace{4mm}
}
%
\newcommand{\subproblem}[1]{
  \vspace{2mm}
  \setlength{\HPHHW@CurrentSubProblemWidth}{\linewidth}
  \addtolength{\HPHHW@CurrentSubProblemWidth}{-\HPHHW@ProblemIndent}
  \begin{minipage}[t]{\HPHHW@ProblemIndent}
    \addtocounter{HPHHW@SubProblemNumberCounter}{1}%
    \alph{HPHHW@SubProblemNumberCounter}.%
  \end{minipage}
  \begin{minipage}[t]{\HPHHW@CurrentSubProblemWidth}
    #1%
  \end{minipage}
}


%hph+
%%
%\newlist{MultipleChoiceOptionList}{enumerate}{1}
%\setlist[MultipleChoiceOptionList]{align=left,label=\alph*.,labelwidth=3mm,leftmargin=!}
%\newtoggle{FirstMultipleChoiceOption}
%\toggletrue{FirstMultipleChoiceOption}
%\newenvironment{MultipleChoiceOption}
%{
%  \vspace{2mm}
%  \iftoggle{FirstMultipleChoiceOption}%
%  {
%    \begin{MultipleChoiceOptionList}[series=numsubproblems]%
%    \global\togglefalse{FirstMultipleChoiceOption}%
%  }{
%    \begin{MultipleChoiceOptionList}[resume=numsubproblems]%
%    \global\togglefalse{FirstMultipleChoiceOption}%
%  }
%  \item\ignorespaces%
%}{
%  \end{MultipleChoiceOptionList}
%  \vspace{4mm}
%}
%%
%hph-


%
% Define the \point, \points, and \printtotalpoints commands.
\newcommand{\point}[1]{
  (#1 point)\addtocounter{HPHHW@ProblemValueSum}{#1}%
}
%
\newcommand{\points}[1]{
  (#1 points)\addtocounter{HPHHW@ProblemValueSum}{#1}%
}
%
\newcommand{\printtotalpoints}{
  \vspace{25mm}
  {\textbf{\textit{The total number of points is \theHPHHW@ProblemValueSum.}}}
}
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Section 7. Settings for multiple choice problems and
%%             settings for solutions layout.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
\newcommand{\HPHHW@hide}[1]{
  \phantom{\varwidth{\linewidth}#1\endvarwidth}
}
%
\newcommand{\solution}[1]{
  \vspace{2mm}
  \ifthenelse{\boolean{HPHHW@SolutionsVersion}}
  {
    {\color{blue} #1}
  }{\ifthenelse{\boolean{HPHHW@SpaceForSolutions}}
    {
      \HPHHW@hide{#1}
    }{
    }
  }
}
%
\newcommand{\multiplechoicesolution}[1]{
  \ifthenelse{\boolean{HPHHW@SolutionsVersion}}
  {{\color{blue}\boxed{#1}}}{#1}
}
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Section 8. Macros for convenient typesetting...these
%%             are mostly for quantum mechanics/chemistry
%%             symbols, etc. The list of available macros
%%             includes:
%%                 \lceil,\rceil
%%                 \lfloor,\rfloor
%%                 \sgn
%%                 \negsign
%%                 \EE{N}       This gives \times{}10^{N}
%%                 \degree
%%                 \updownarrow
%%                 \commutator
%%                 \anticommutator
%%                 \deter
%%                 \bra
%%                 \ket
%%                 \expect
%%                 \braket
%%                 \ketbra
%%                 \braketdoubar
%%                 \braketop
%%                 \spatialbraketop
%%                 \eri
%%                 \eridoubar
%%                 \orb
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
\DeclarePairedDelimiter{\ceil}{\lceil}{\rceil}

\DeclarePairedDelimiter{\floor}{\lfloor}{\rfloor}

\DeclareMathOperator{\sgn}{sgn}

\newcommand{\negsign}{
  {\ensuremath
    {\scalebox{1.25}[1.2]{-}}
  }
}

\newcommand{\EE}[1]{
  {\ensuremath
    {\times{}10^{#1}}
  }
}

\providecommand{\degree}{\ensuremath{^\circ}}

\providecommand{\updownarrow}{abc}
\renewcommand{\updownarrow}
  {\ensuremath
    {\uparrow\downarrow}%
  }

\providecommand{\commutator}{abc}
\renewcommand{\commutator}[2]
  {\ensuremath
    {\left[ #1,#2 \right]}%
  }

\providecommand{\anticommutator}{abc}
\renewcommand{\anticommutator}[2]
  {\ensuremath
    {\left\{ #1,#2 \right\}}%
  }

\providecommand{\deter}{abc}
\renewcommand{\deter}[1]
  {\ensuremath
    {\left| #1 \right|}%
  }

\providecommand{\bra}{abc}
\renewcommand{\bra}[1]
  {\ensuremath
    {\left\langle #1 \right|}%
  }

\providecommand{\ket}{abc}
\renewcommand{\ket}[1]
  {\ensuremath
    {\left| #1 \right\rangle}%
  }

\providecommand{\expect}{abc}
\renewcommand{\expect}[1]
  {\ensuremath
    {\left\langle #1 \right\rangle}%
  }

\providecommand{\braket}{abc}
\renewcommand{\braket}[2]
  {\ensuremath
    {\left\langle { #1 }
      \mathrel{\left | {\vphantom { #1 #2 }}
      \right. \kern-\nulldelimiterspace}
      { #2 } \right\rangle}%
  }

\providecommand{\ketbra}{abc}
\renewcommand{\ketbra}[2]
  {\ensuremath
    {\left| { #1 }
      \mathrel{\left\rangle {\vphantom { #1 #2 }}
      \right\langle \kern-\nulldelimiterspace}
      { #2 } \right|}%
  }

\providecommand{\braketdoubar}{abc}
\renewcommand{\braketdoubar}[2]
  {\ensuremath
    {\left\langle { #1 }
      \mathrel{\left | {\vphantom { #1 #2 }}
      \right| \kern-\nulldelimiterspace}
      { #2 } \right\rangle}%
  }

\providecommand{\braketop}{abc}
\renewcommand{\braketop}[3]
  {\ensuremath
    {\left\langle { #1 }
      \mathrel{\left | {\vphantom { #1 #2 #3 }} #2
      \right| \kern-\nulldelimiterspace}
      { #3 } \right\rangle}%
  }

\providecommand{\spatialbraketop}{abc}
\renewcommand{\spatialbraketop}[3]
  {\ensuremath
    {\left( { #1 }
      \mathrel{\left | {\vphantom { #1 #2 #3 }} #2
      \right| \kern-\nulldelimiterspace}
      { #3 } \right)}%
  }

\providecommand{\eri}{abc}
\renewcommand{\eri}[2]
  {\ensuremath
    {\left( { #1 }
      \mathrel{\left | {\vphantom { #1 #2 }}
      \right. \kern-\nulldelimiterspace}
      { #2 } \right)}%
  }

\providecommand{\eridoubar}{abc}
\renewcommand{\eridoubar}[2]
  {\ensuremath
    {\left( { #1 }
      \mathrel{\left | {\vphantom { #1 #2 }}
      \right| \kern-\nulldelimiterspace}
      { #2 } \right)}%
  }

\providecommand{\orb}{abc}
\renewcommand{\orb}[3]
  {\ensuremath
    {#1_{#2}\left(#3\right)}
  }
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Section 9. Macros and settings for section headers and other general
%%             typesetting options.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Set section heading formatting.
%
\titleformat{\section}{\sffamily\normalsize\scshape\bfseries}{\thesection.}{0.2em}{}
\titlespacing{\section}{0pt}{7mm}{1mm}
\titleformat{\subsection}{\normalsize\bfseries}{}{0.2em}{}
\titlespacing{\subsection}{0pt}{5mm}{1mm}
\titleformat{\subsubsection}[runin]{\normalsize\slshape\bfseries}{}{0em}{}[.]
\titlespacing{\subsubsection}{0pt}{*2}{*1}
%
% Define the changemargin command.
%
\def\changemargin#1#2{\list{}{\rightmargin#2\leftmargin#1}\item[]}
\let\endchangemargin=\endlist 
