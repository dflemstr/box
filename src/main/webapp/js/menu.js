$(function() {
    $('.menu-dropdown-arrow').click(function() {
        var self = $(this), parent = self.parent(), submenu = parent.find('.submenu');
        self.addClass('open');
        submenu.slideDown('fast').show();
        parent.mouseleave(function() {
            submenu.slideUp('slow', function() {
                self.removeClass('open');
            });
        });
    }).hover(function() {
        $(this).addClass('hover');
    }, function() {
        $(this).removeClass('hover');
    });
});