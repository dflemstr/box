var maybeLoadMore = function() {
    var nativeWind = window,
    wind = $(nativeWind),
    loader = $('#lazy-loader').parent(),
    windJQHeight = wind.height(),
    windBottom = wind.scrollTop() + ((windJQHeight == 0) ? nativeWind.innerHeight : windJQHeight), //Height workaround for Chrome 5 and 6
    loaderTop = loader.offset().top;
    if(loaderTop < windBottom + 200) {
        loader.trigger('loadmore');
    }
}

var bindLoadEvent = function(toId, callback) {
    $('#' + toId).one('loadmore', callback);
    maybeLoadMore();
}

$(function() {
    $(window).bind('scroll resize', maybeLoadMore);
});
