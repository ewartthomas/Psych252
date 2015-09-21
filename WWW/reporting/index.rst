Reporting statistics
=======================

This is a collection of examples demonstrating how to report statistics, including the text
and R Markdown syntax, as well as examples from journal articles.

Created by Psych 252 TAs

Chi-squared test
---------------------------
[`Examples <http://www.stanford.edu/class/psych252/reporting/examples/chisq.html>`_]

.. math::

  \chi^2 (3, N=328) = 11.9, p < 0.01

::

  $\chi^2$ (`r rs$parameter`, N=`r sum(rs$observed)`) = `r rs$statistic`, p = `r rs$p.value`


---------------------------
