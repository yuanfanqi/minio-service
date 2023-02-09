package com.minioservice.commonModel;

import com.minioservice.enums.ResponseCode;

public class Result<T> {

    /** 状态码 */
    private Integer code;
    /** 返回信息msg */
    private String msg;
    /** 返回信息data */
    private T data;

    public Result() {

    }

    public Result(Integer code, String msg, T data) {
        this.code = code;
        this.msg = msg;
        this.data = data;
    }

    public Result(Integer code, T data) {
        this.code = code;
        this.data = data;
    }

    public Result(Integer code, String msg) {
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
    public Result ok(String msg) {
        return new Result<>(ResponseCode.SUCCESS, msg);
    }
    /**
     * @Description: 返回成功
     * @Author: song
     * @Date: 2023/2/9 13:59
     */
    public Result ok(T data) {
        return new Result<>(ResponseCode.SUCCESS, data);
    }

    /**
     * @Description: 返回失败
     * @Author: song
     * @Date: 2023/2/9 13:59
     */
    public Result fail(String msg) {
        return new Result<>(ResponseCode.SUCCESS, msg);
    }
    /**
     * @Description: 返回失败
     * @Author: song
     * @Date: 2023/2/9 13:59
     */
    public Result fail(T data) {
        return new Result<>(ResponseCode.SUCCESS, data);
    }


}
