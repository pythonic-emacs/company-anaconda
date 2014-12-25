.. |melpa| image:: http://melpa.org/packages/company-anaconda-badge.svg
    :target: http://melpa.org/#/company-anaconda
    :alt: Melpa

================
Company anaconda
================

|melpa|

Anaconda_ backend for company-mode_.

.. figure:: static/company-anaconda.png

Installation
------------

You can install this package from Melpa_::

    M-x package-install RET company-anaconda RET

Usage
-----

Add ``company-anaconda`` to allowed ``company-mode`` backends list

.. code:: lisp

    (add-to-list 'company-backends 'company-anaconda)

.. _Anaconda: https://github.com/proofit404/anaconda-mode
.. _company-mode: http://company-mode.github.io/
.. _Melpa: http://melpa.milkbox.net/
