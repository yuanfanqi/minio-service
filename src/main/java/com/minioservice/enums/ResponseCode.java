package com.minioservice.enums;


public class ResponseCode {
    /** 请求成功 */
    public static final Integer SUCCESS = 200;
    /** 请求失败 */
    public static final Integer FAIL = 500;
    /** 必须要有token */
    public static final Integer AC = 102;
    /** 令牌不能为空 */
    public static final Integer TOKEN_IS_EMPTY = 401;
    /** 参数列表错误（缺少，格式不匹配） */
    public static final Integer BAD_REQUEST = 400;
    /** prodCode不能为空 */
    public static final Integer PRODCODE_IS_EMPTY = 450;
}
