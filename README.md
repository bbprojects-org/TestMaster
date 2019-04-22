## Introduction

In 1998 whilst undertaking my training for Microsoft Certified Systems Engineer (MCSE), I developed a program to create, edit and execute multiple-choice questions… TestMaster. It was written in Pascal using Borland Delphi.

Many people have used it over the years and I had reason recently to resurrect it, although now I am Mac based and use Lazarus/FPC in lieu of Delphi, so it got ported and some rework too.

TestMaster provides the following features:

- Central control of all question sets with summary information for each
- Optional exhibit (graphic) for each question
- Answer choices can be single or multiple answers
- Option to randomise question order
- Option to randomise answer choices order (question by question)
- Ability to set questions as 'marked' and go back to them
- Option to 'preview' answers, with explanatory notes (if provided)
- On screen time indication; either elapsed time or time remaining
- Maintained results summary of tests taken, results and which questions were answered incorrectly (this permits individuals to monitor their progress over time)

Some of the features of the original program have been removed as they are no longer relevant to open-source software:

- restriction if not-registered
- encrypted data files

## License

This software has been released under the [GNU General Public License](https://www.gnu.org/licenses/) as published by the Free Software Foundation
