package com.minioservice.enums;


public class MsgEnum {
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
    /** 上传文件不能为空 */
    public static final String FILE_IS_EMPTY = "上传失败：上传文件不能为空";
    /** 上传文件已存在，且未授权覆盖 */
    public static final String NO_AUTH_REPLACE_FILE = "上传失败：上传文件已存在，且未授权覆盖";
    /** 上传失败：minIO建立连接失败 */
    public static final String MINIO_CONNECT_FAIL = "上传失败：minIO建立连接失败";
    /** 文件名参数缺失 */
    public static final String FILE_NAME_IS_EMPTY = "文件名参数缺失";
    /** 文件不存在 */
    public static final String FILE_IS_NOT_EXIST = "文件不存在";
    /** 文件tags参数缺失 */
    public static final String TAGS_IS_EMPTYT = "文件tags参数缺失";

}
