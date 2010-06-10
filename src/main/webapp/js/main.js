var startAjax = function() {
    //Called whenever the app starts an AJAX poll
    $('#ajax-indicator').fadeIn('slow');
}

var stopAjax = function() {
    //Called whenever the app finishes an AJAX poll
    $('#ajax-indicator').fadeOut('fast');
}

var displayNotice = function(kind, title, body, assocId) {
    //Called when the user gets new messages
    $.gritter.add({
        title: title,
        text: body,
        image: '/images/' + kind + '-icon.png',
        sticky: (kind == 'error'),
        time: 8000,
        class_name: kind + '-notification ui-corner-all'
    });
}
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
