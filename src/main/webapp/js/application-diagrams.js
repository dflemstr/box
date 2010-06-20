$(function() {
    $.plot($('#history-diagram'),[
    {
        data: hu,
        label: locHu,
        color: '#C3D9FF',
        lines: {
            show: true,
            fill: true
        }
    }
    ], {
        xaxis: {
            mode: "time"
        },
        yaxis: {
            tickDecimals: 0,
            min: 0
        }
    });
});
