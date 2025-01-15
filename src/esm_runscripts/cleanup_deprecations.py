import warnings
from functools import wraps
from typing import Any


def deprecated_class(cls):
    original_init = getattr(cls, "__init__", None)
    class_name = cls.__name__
    parent_class_name = cls.__bases__[0].__name__
    message = f"The '{class_name}' class is deprecated. Please update your code to use the '{parent_class_name}' class instead."

    @wraps(cls.__init__ if original_init else lambda self, *args, **kwargs: None)
    def new_init(self, *args: Any, **kwargs: Any) -> None:
        warnings.warn(
            message,
            DeprecationWarning,
            stacklevel=2,
        )
        if original_init:
            original_init(self, *args, **kwargs)
        else:
            super(cls, self).__init__(*args, **kwargs)

    cls.__init__ = new_init
    return cls
