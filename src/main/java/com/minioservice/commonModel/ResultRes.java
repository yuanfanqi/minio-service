package com.minioservice.commonModel;

import com.minioservice.enums.MsgEnum;

public class ResultRes<T> {

    /** 状态码 */
    private Integer code;
    /** 返回信息msg */
    private String msg;
    /** 返回信息data */
    private T data;

    public ResultRes() {
    }

    public ResultRes(Integer code, String msg, T data) {
        this.code = code;
        this.msg = msg;
        this.data = data;
    }

    public ResultRes(Integer code, T data) {
        this.code = code;
        this.data = data;
    }

    public ResultRes(Integer code, String msg) {
        this.code = code;
        this.msg = msg;
    }

    public Integer getCode() {
        return code;
    }

    public void setCode(Integer code) {
        this.code = code;
    }

    public String getMsg() {
        return msg;
    }

    public void setMsg(String msg) {
        this.msg = msg;
    }

    public T getData() {
        return data;
    }

    public void setData(T data) {
        this.data = data;
    }

    /**
     * @Description: 返回成功
     * @Author: song
     * @Date: 2023/2/9 13:59
     */
    public static <T> ResultRes ok(String msg) {
        return new ResultRes<>(MsgEnum.SUCCESS, msg);
    }
    /**
     * @Description: 返回成功
     * @Author: song
     * @Date: 2023/2/9 13:59
     */
    public static <T> ResultRes ok(T data) {
        return new ResultRes<>(MsgEnum.SUCCESS, data);
    }

    /**
     * @Description: 返回失败
     * @Author: song
     * @Date: 2023/2/9 13:59
     */
    public static ResultRes fail(String msg) {
        return new ResultRes<>(MsgEnum.FAIL, msg);
    }
    /**
     * @Description: 返回失败
     * @Author: song
     * @Date: 2023/2/9 13:59
     */
    public static <T> ResultRes fail(T data) {
        return new ResultRes<>(MsgEnum.FAIL, data);
    }


}
